#include <Rcpp.h>
using namespace Rcpp;

/*
  * codes in sdc_status:
  * 0: possible suppression partner ("s")
  * 1: primary suppression ("u")
  * 2: secondary suppression ("x")
  * 3: non-applicable ("z")
  * 4: dummy-cells ("w")
*/
IntegerVector sdcstatus_to_num(CharacterVector sdc_status) {
  IntegerVector result(sdc_status.size());
  for (int i = 0; i < sdc_status.size(); i++) {
    if (sdc_status[i] == "s") {
      result[i] = 0;
    }
    if (sdc_status[i] == "u") {
      result[i] = 1;
    }
    if (sdc_status[i] == "x") {
      result[i] = 2;
    }
    if (sdc_status[i] == "z") {
      result[i] = 3;
    }
    if (sdc_status[i] == "w") {
      result[i] = 4;
    }
  }
  return(result);
}

// [[Rcpp::export]]
List greedyMultDimSuppression(DataFrame dat, List indices, List subIndices, IntegerVector dimVars, bool verbose) {
  bool debug = true;
  Function cpp_print("print");

  /* start protection of data() */
  int nr_dims = dimVars.size();
  if (verbose == true) {
    Rcout << "We have to protect an " << nr_dims << " dimensional dataset." << std::endl;
  }
  
  /* extracting input data from list */
  IntegerVector freq = dat["freq"];
  NumericVector weights = dat["weights"];
  CharacterVector sdcStatus = dat["sdcStatus"];
  IntegerVector sdcstatus_num = sdcstatus_to_num(sdcStatus);
  IntegerVector id = dat["id"];

  // is_real_z is true for all cells that are initially "z"
  LogicalVector is_real_z = sdcstatus_num == 3;

  IntegerVector current_indices, st_freq, st_sdcStatus_num, st_id;
  NumericVector st_weights;
  CharacterVector st_sdcstatus;
  LogicalVector st_is_real_z;
  List st_subindices, sub_list;
  int nr_groups = indices.size();
  int ind_x = -1;
  bool runInd = true;
  int counter = 1;
  int total_new_supps = 0; /* total number of required secondary supps */
  while (runInd == true) {
    if (verbose) {
      Rcout << "Run: " << counter << std::endl;
    }
    bool override = false;
    bool final_ok = true;
    for (int group = 0; group < nr_groups; group++) {
      List sub_list = indices[group];
      int nr_tabs = sub_list.size();
      /* tab is iterating over all subtables within a given group */
      for (int tab = 0; tab < nr_tabs; tab++) {
        /* subsetting data to current subtable */
        List sub_list = indices[group];
        current_indices = sub_list[tab];
        int n_st = current_indices.size();

        /* Select freqs and suppression pattern of current subtable */
        st_freq = freq[current_indices - 1];
        st_weights = weights[current_indices - 1];
        st_sdcstatus = sdcStatus[current_indices - 1];
        st_sdcStatus_num = sdcstatus_num[current_indices - 1];
        st_id = id[current_indices - 1];
        st_is_real_z = is_real_z[current_indices - 1]; /* ids in entire dataset, c-style (starting with 0!) */
        st_id = st_id - 1;

        IntegerVector sub_ids = seq_along(st_id);
        sub_ids = sub_ids - 1;

        /* create list for indices defining subtables */
        List tmp_subindices = subIndices[group];
        st_subindices = tmp_subindices[tab];

        /* iterativly protecting the current simple tab */
        bool newsupps_added = true;
        int supps_added = 0;
        while (newsupps_added == true) {
          int additional_supps = 0;
          for (int i = 0; i < nr_dims; i++) {
            IntegerVector gr = st_subindices[i];
            int nr_simple_tabs = max(gr);
            IntegerVector c_indices = seq_along(gr) - 1; // indices relative to current simple table, c-style!

            for (int j = 0; j < nr_simple_tabs; j++) {
              /* subset original vectors to current subtable */
              /* ind_c: index relative to entire data, starting with 0 */
              IntegerVector ind_c = c_indices[gr==j+1];
              int nr_cells = ind_c.size();
              if (nr_cells > 1) {
                /* extract values of current simple table */
                IntegerVector cur_freq = st_freq[ind_c];
                NumericVector cur_weights = st_weights[ind_c];
                NumericVector cur_weights_o = clone(cur_weights);
                IntegerVector cur_sdcstatus = st_sdcStatus_num[ind_c];
                IntegerVector cur_id = st_id[ind_c];
                IntegerVector cur_sub_ids = sub_ids[ind_c];
                LogicalVector cur_is_real_z = st_is_real_z[ind_c];
                LogicalVector is_candidate(cur_freq.size());

                if (debug) {
                  Rcout << "current subtable as data.frame (cur_st_df)" << std::endl;
                  DataFrame cur_st_df = DataFrame::create(
                    Named("cur_freq")=cur_freq,
                    Named("cur_weights")=cur_weights,
                    Named("cur_weights_o")=cur_weights_o,
                    Named("cur_sdcstatus")=cur_sdcstatus,
                    Named("cur_id")=cur_id,
                    Named("cur_sub_ids")=cur_sub_ids,
                    Named("cur_is_real_z")=cur_is_real_z);
                  print(cur_st_df);
                }

                /* calculating the number of suppressed cells */
                int nr_supps = 0;
                int up_val = max(cur_weights) + 1.0;
                int nr_dummycells = 0;
                int nr_zcells = 0;
                for (int kk = 0; kk < nr_cells; kk++) {
                  is_candidate[kk] = false;
                  if ((cur_sdcstatus[kk] == 1) or (cur_sdcstatus[kk]==2)) {
                    nr_supps = nr_supps + 1;
                    cur_weights[kk] = up_val;
                  }
                  /* we only count 'z' cells as a candiate that were not initially z */
                  if (cur_sdcstatus[kk] == 3) {
                    nr_zcells = nr_zcells + 1;
                  }
                  if (cur_sdcstatus[kk] == 4) {
                    nr_dummycells = nr_dummycells + 1;
                  }
                  if ((cur_sdcstatus[kk] == 0) & (cur_freq[kk] > 0)) {
                    is_candidate[kk] = true;
                  }
                }
                /*
                  we have at least 1 observation in the current
                  simple table but only a single suppressed cell and
                  the number of dummy-cells (which should never be published) is 0
                */
                if ((nr_supps == 1) & (nr_dummycells == 0)) {
                  int nr_candidates = sum(is_candidate);
                  if (nr_candidates == 0) {
                    if (nr_zcells == 0) {
                      if (debug) {
                        DataFrame debug_df = DataFrame::create(
                          Named("nr_cells") = nr_cells,
                          Named("nr_supps") = nr_supps,
                          Named("nr_dummycells") = nr_dummycells,
                          Named("cur_freq") = cur_freq,
                          Named("cur_weights") = cur_weights,
                          Named("cur_weights_o") = cur_weights_o,
                          Named("cur_sdcstatus") = cur_sdcstatus);
                        print(debug_df);
                      }
                      stop("Unfortunately, it is not possible to find a suppression pattern.");
                    }

                    /*
                      Relaxation case
                      In this case, it is not possible to find a pattern with only 's'-cells.
                      we need to relax 'z' (code 3) cells to 's' (code 0) and try again.
                      However, we do not allow z-cells that were initially "z"
                    */
                    if (debug) {
                      Rcout << "we need to set 'z'-cells to 's'!" << std::endl;
                      Rcout << "the current subtable (debug_df):" << std::endl;
                      DataFrame debug_df = DataFrame::create(
                        Named("nr_cells") = nr_cells,
                        Named("nr_supps") = nr_supps,
                        Named("nr_dummycells") = nr_dummycells,
                        Named("cur_freq") = cur_freq,
                        Named("cur_weights") = cur_weights,
                        Named("cur_weights_o") = cur_weights_o,
                        Named("cur_sdcstatus") = cur_sdcstatus,
                        Named("cur_real_z") = cur_is_real_z);
                      print(debug_df);
                    }
                    override = true;
                    LogicalVector ii = (cur_sdcstatus==3) & (cur_is_real_z == false);

                    // check if we have candidates; if not we stop with an error
                    // as we do not want to change pre-existing z-cells
                    if (sum(ii) == 0) {
                      DataFrame debug_df = DataFrame::create(
                        Named("cur_id") = cur_id,
                        Named("cur_freq") = cur_freq,
                        Named("cur_weights") = cur_weights,
                        Named("cur_weights_o") = cur_weights_o,
                        Named("cur_sdcstatus") = cur_sdcstatus,
                        Named("cur_real_z") = cur_is_real_z);
                      print(debug_df);
                      stop("No valid suppression pattern can be found (too many initial z-cells?)");
                    }

                    cur_weights[ii] = cur_weights_o[ii]; // reset to original value
                    cur_sdcstatus[ii] = 0; // set cur_sdcstatus temporarily to 's' (0)
                    if (debug) {
                      Rcout << "relaxdebug_df:" << std::endl;
                      DataFrame relaxdebug_df = DataFrame::create(
                        Named("ii") = ii,
                        Named("cur_freq") = cur_freq,
                        Named("cur_weights") = cur_weights,
                        Named("cur_weights_o") = cur_weights_o,
                        Named("cur_sdcstatus") = cur_sdcstatus,
                        Named("cur_is_real_z") = cur_is_real_z,
                        Named("cur_sub_ids") = cur_sub_ids);
                      print(relaxdebug_df);
                    }
                    is_candidate = (cur_sdcstatus == 0);
                  }

                  /* show output on current simple table */
                  if (debug) {
                    Rcout << "subtable_df:" << std::endl;
                    DataFrame subtable_df = DataFrame::create(
                      Named("cur_id") = cur_id,
                      Named("cur_freq") = cur_freq,
                      Named("cur_weights") = cur_weights,
                      
                      Named("cur_sub_ids") = cur_sub_ids,
                      Named("is_candidate") = is_candidate,
                      Named("cur_sdcstatus") = cur_sdcstatus,
                      Named("is_real_z") = cur_is_real_z);
                    print(subtable_df);
                    R_FlushConsole();
                  }

                  /* Restrict to candidate cells */
                  cur_id = cur_id[is_candidate];
                  cur_freq = cur_freq[is_candidate];
                  cur_weights = cur_weights[is_candidate];
                  cur_sub_ids = cur_sub_ids[is_candidate];
                  cur_sdcstatus = cur_sdcstatus[is_candidate];
                  cur_is_real_z = cur_is_real_z[is_candidate];

                  /*
                    find the index with lowest weights but take the
                    highest index if multiple obs exist!
                    This helps to prevent suppressing marginal cells
                  */
                  double min_val = min(cur_weights);
                  IntegerVector v = seq(0, cur_weights.size() - 1);
                  IntegerVector tmpres = v[cur_weights == min_val];
                  ind_x = max(tmpres);
                  int final_index = cur_id[ind_x]; // c-indices
                  int final_st_index = cur_sub_ids[ind_x];

                  /* check if suppressed cell is > 0! */
                  if (freq[final_index] == 0) {
                    stop("frequency of suppressed cell is 0!");
                  }

                  /* print info about newly suppressed cell */
                  if (debug) {
                    Rcout << "--> Suppression found" << std::endl;
                    DataFrame df_supp = DataFrame::create(
                      Named("id") = final_index,
                      Named("freq") = freq[final_index],
                      Named("weights") = weights[final_index],
                      Named("sdcstatus_num") = sdcstatus_num[final_index]);
                    print(df_supp);
                    R_FlushConsole();
                  }

                  additional_supps = additional_supps + 1;

                  /* add suppressions to required vectors (also to current subtable!) */
                  sdcStatus[final_index] = "x";
                  sdcstatus_num[final_index] = 2;
                  st_sdcStatus_num[final_st_index] = 2;
                  supps_added = supps_added + 1;
                } else {
                  // the single suppressed cell is protected by a dummy-cell that
                  // must never be published!
                }
              } else {
                // nothing to do for subtables with only one cell
              }
            } /* end j-loop (simple tables) */
          } /* end i-loop for groups */

          if (additional_supps == 0) {
            /* we can stop the inner loop and move to the next subtable */
            if (verbose == true) {
              Rcout << "Group: " << group + 1 <<"|" << nr_groups << ": ";
              Rcout << "subTable: " << tab + 1 <<"|" << nr_tabs;
              Rcout << " | nrCells: " << n_st;
              if (supps_added == 0) {
                Rcout << " | everything ok/nothing todo." << std::endl;
              } else {
                Rcout << " | additionally suppressed cells: " << supps_added << std::endl;
              }
              R_FlushConsole();
            }
            newsupps_added = false;
            total_new_supps = total_new_supps + supps_added;
          } else {
            final_ok = false;
          }
        } /* inner while()-loop */
      } /* end for-loop (tabs) */

      /* set all 's' cells in this subtable to 'z' */
      for (int x1 = 0; x1 < st_freq.size(); x1++) {
        if ((st_sdcStatus_num[x1] == 0) and st_freq[x1] > 0) {
          int ind = st_id[x1];
          sdcstatus_num[ind] = 3;
          sdcStatus[ind] = "z";
        }
      }
    } /* end for-loop (groups) */

    /*
      check if we went through all subtables in all groups
      without setting any 'z' cells to 's'.
      In this case, we can stop the outer while-loop!
    */
    if ((override == false) & (final_ok == true)) {
      runInd = false;
    } else {
      counter = counter + 1;
      /* we need to set all 'z' cells back to to 's' */
      LogicalVector vv1 = ((sdcstatus_num == 0) | (sdcstatus_num == 3)) & (is_real_z == false);
      sdcstatus_num[vv1] = 0;
      sdcStatus[vv1] = "s";

      LogicalVector vv2 = ((sdcstatus_num == 0) | (sdcstatus_num == 3)) & (is_real_z == false);
      sdcstatus_num[vv2] = 3;
      sdcStatus[vv2] = "z";
    }
  }

  if (verbose == true) {
    Rcout << "Finished. Total number of new suppressions: " << total_new_supps << std::endl;
  }
  IntegerVector total_new_suppsv(1);
  total_new_suppsv[0] = total_new_supps;
  
  return Rcpp::List::create(
    Rcpp::Named("id") = id,
    Rcpp::Named("freq") = freq,
    Rcpp::Named("sdcStatus") = sdcStatus,
    Rcpp::Named("total_new_supps") = total_new_suppsv);
}
