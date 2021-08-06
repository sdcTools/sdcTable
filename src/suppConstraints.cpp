#include <Rcpp.h>
using namespace Rcpp;

List simple_triplet_to_indices(List mat, LogicalVector is_common_cell, IntegerVector freqs) {
  Function cpp_print("print");

  // looping over the constraint matrix"
  List m = clone(mat);
  IntegerVector vals_i = m["i"]; // row-indices
  IntegerVector vals_j = m["j"]; // column-indices
  IntegerVector vals_v = m["v"]; // -1: total; 1: contributing value
  int nr_constraints = max(vals_i);

  // pre-allocate results
  List ll(nr_constraints);
  LogicalVector contains_common(nr_constraints);
  LogicalVector contains_singletons(nr_constraints);

  LogicalVector is_protected = rep(false, nr_constraints);

  LogicalVector col_ind;
  IntegerVector mat_cols;
  IntegerVector vals;

  for (int i = 1; i <= nr_constraints; i++) {
    col_ind = vals_i == i;
    mat_cols = vals_j[col_ind];
    mat_cols = mat_cols - 1; // cpp-index
    vals = vals_v[col_ind];

    // check if the constraint contains common cells
    LogicalVector to_check = is_common_cell[mat_cols];
    if (sum(to_check) > 0) {
      contains_common[i - 1] = true;
    } else {
      contains_common[i - 1] = false;
    }

    // check if the constraint contains singletons
    IntegerVector cc_freqs = freqs[mat_cols];
    LogicalVector cc_is_singleton = cc_freqs == 1;
    if (sum(cc_is_singleton) > 0) {
      contains_singletons[i - 1] = true;
    } else {
      contains_singletons[i - 1] = false;
    }

    List tmplist = List::create(
      Named("idx") = mat_cols,
      Named("vals") = vals
    );
    ll[i - 1] = tmplist;
  }

  IntegerVector constraint_type = rep(1, nr_constraints);
  List res = List::create(
    Named("constraints") = ll,
    Named("contains_common") = contains_common,
    Named("contains_singletons") = contains_singletons,
    Named("is_protected") = is_protected,
    Named("nr_constraints") = nr_constraints,
    Named("constraint_type") = constraint_type
  );
  return(res);
}

List find_linked_constraints(List constraints, IntegerVector freqs, LogicalVector is_common_cell, bool verbose) {
  // we use the result from  `simple_triplet_to_indices()` as input and compute additional constraints
  Function cpp_print("print");
  List output;

  List cc = clone(constraints);
  int nr_constraints = cc.length();
  int counter = 0;
  IntegerVector jvec = {-1, 1, 1};

  IntegerVector vvec, vval;
  for (int i = 0; i < (nr_constraints - 1); i++) {
    for (int j = i + 1; j < nr_constraints; j++) {
      counter = counter + 1;
      List inp_i = cc[i];
      List inp_j = cc[j];

      IntegerVector idx_i = inp_i["idx"];
      IntegerVector idx_j = inp_j["idx"];

      // total and contributing indices
      IntegerVector vind = {0};
      IntegerVector idx_tot_i = idx_i[vind];
      IntegerVector idx_tot_j = idx_j[vind];

      IntegerVector idx_contr_i = setdiff(idx_i, idx_tot_i);
      IntegerVector idx_contr_j = setdiff(idx_j, idx_tot_j);

      // constraint generation //
      IntegerVector vint = intersect(idx_contr_i, idx_contr_j);
      IntegerVector vposs = union_(idx_contr_i, idx_contr_j);

      IntegerVector overlapping_ids = setdiff(vposs, vint);
      if (overlapping_ids.size() == 1) {
        bool ccell = is_common_cell[overlapping_ids];
        // the overlapping cell is a common-cell
        if (ccell == true) {
          int overlap_id = overlapping_ids[0];
          IntegerVector val_i = freqs[idx_tot_i];
          int tot_i = val_i[0];
          IntegerVector val_j = freqs[idx_tot_j];
          int tot_j = val_j[0];
          if (tot_i >= tot_j) {
            IntegerVector vvec = {idx_tot_i[0], idx_tot_j[0], overlap_id};
            List ll = List::create(
              Named("idx") = vvec,
              Named("vals") = jvec
            );
            output.push_back(ll);
          } else {
            IntegerVector vvec = {idx_tot_j[0], idx_tot_i[0], overlap_id};
            List ll = List::create(
              Named("idx") = vvec,
              Named("vals") = jvec
            );
            output.push_back(ll);
          }
        }
      }

      if (overlapping_ids.size() == 2) {
        // check if all overlapping cells are from the same constraint
        std::vector<int> v_chk = Rcpp::as<std::vector<int> >(idx_contr_i);
        std::vector<int>::iterator it;
        LogicalVector l1 = { false, false};
        int chkval;
        for (int j = 0; j < 2; j++) {
          chkval = overlapping_ids[j];
          it = std::find(v_chk.begin(), v_chk.end(), chkval);
          if (it != v_chk.end()) {
            l1[j] = true;
          }
        }
        int sum1 = sum(l1);
        if (sum1 == 0) {
          // case1: both contributing overlapping cells come from constraint j
          vvec = {idx_tot_i[0], idx_tot_j[0], overlapping_ids[0], overlapping_ids[1]};
          vval = {-1, 1, -1, -1};
        } else if (sum1 == 2) {
          // case2: both contributing overlapping cells come from constraint i
          vvec = {idx_tot_j[0], idx_tot_i[0], overlapping_ids[0], overlapping_ids[1]};
          vval = {-1, 1, -1, -1};
        } else {
          // case3: one overlapping cell is in constraint i, the other in j
          bool from_con_a = l1[0];
          vvec = {idx_tot_i[0], idx_tot_j[0], overlapping_ids[0], overlapping_ids[1]};
          if (from_con_a == true) {
            vval = {-1, 1, 1, -1};
          } else {
            vval = {-1, 1, -1, 1};
          }
        }
        List ll = List::create(
          Named("idx") = vvec,
          Named("vals") = vval
        );
        output.push_back(ll);
      }
    }
  }
  if (verbose == true) {
    Rcout << output.size() << " new constraints detected | nr_comparisons: " << counter << std::endl;
  }
  return(output);
}

bool check_constraints(List inp) {
  /* returns true if all constraints are valid (summing to zero) */
  List con = inp["constraints"];
  IntegerVector freqs = inp["freqs"];
  int nr_constraints = inp["nr_constraints"];
  LogicalVector res = rep(false, nr_constraints);

  for (int i = 0; i < nr_constraints; i++) {
    List cur_constraint = con[i];
    IntegerVector idx = cur_constraint["idx"];
    IntegerVector f = freqs[idx];
    IntegerVector v =  cur_constraint["vals"];
    IntegerVector sumvec = v * f;
    int summe = sum(sumvec);
    if (summe == 0) {
      res[i] = true;
    } else {
      Rcout << "problem mit constraint " << i << " | summe: " << summe << std::endl;
      res[i] = false;
    }
  }
  int sum_ok = sum(res);
  bool ok = false;
  if (sum_ok == nr_constraints) {
    ok = true;
  }
  return(ok);
}

List create_supp_inputs(List mat, LogicalVector is_common_cell, IntegerVector freqs, NumericVector weights, CharacterVector sdc_status, bool find_overlaps, bool verbose) {
  List cc = simple_triplet_to_indices(mat, is_common_cell, freqs);
  if (find_overlaps == true) {
    // subset to constraints that contain at least a single common cell
    // improves performance as it reduces the number of required comparisons
    LogicalVector contains_common = cc["contains_common"];
    List all_constraints = cc["constraints"];
    List constraints_with_commoncells = all_constraints[contains_common];
    List linked_constraints = find_linked_constraints(
      constraints_with_commoncells,
      freqs,
      is_common_cell,
      verbose
    );

    // update
    IntegerVector cc_type = cc["constraint_type"];
    LogicalVector cc_common = cc["contains_common"];
    LogicalVector cc_singletons = cc["contains_singletons"];
    LogicalVector cc_is_protected = cc["is_protected"];
    List cc_con = cc["constraints"];

    int nr_additional_constraints = linked_constraints.size();
    for (int i = 0; i < nr_additional_constraints; i++) {
      cc_type.push_back(2);
      cc_common.push_back(true);

      List xx = linked_constraints[i];
      IntegerVector chk_idx = xx["idx"];
      IntegerVector chk_f = freqs[chk_idx];
      LogicalVector chk_singleton = chk_f == 1;
      bool chk_singletons = sum(chk_singleton) > 0;

      cc_singletons.push_back(chk_singletons);
      cc_is_protected.push_back(false);
      cc_con.push_back(linked_constraints[i]);
    }
    cc["constraint_type"]  = cc_type;
    cc["contains_common"]  = cc_common;
    cc["contains_singletons"] = cc_singletons;
    cc["is_protected"]  = cc_is_protected;
    cc["constraints"]  = cc_con;
    int nr_ex_constraints = cc["nr_constraints"];
    cc["nr_constraints"] = nr_ex_constraints + nr_additional_constraints;
  }

  // constraint_type: 1 -> ordinary constraint
  // constraint_type: 2 -> constraint due to linked tables
  IntegerVector ids = seq_len(freqs.length());
  ids = ids - 1;
  cc.push_back(ids, "ids");
  cc.push_back(freqs, "freqs");
  cc.push_back(weights, "weights");
  cc.push_back(sdc_status, "sdc_status");
  return(cc);
}

bool compare_charvecs(CharacterVector x, CharacterVector y) {
  LogicalVector r(x.size());
  for (int i = 0; i < x.size(); i++) {
    if (x[i] != y[i]) {
      return(false);
    }
  }
  return(true);
}

/* finds a new suppression-index; based on minimal value of input `weights`
 * and returns a vector of length 2 with first element being the global and
 * the second element the local index of the cell that needs to be suppressed
 * weights: weights used to identify a suitable suppession-index
 * idx_global: (global) indices
 * idx_local: (local) indices
 */
IntegerVector find_additional_suppression(NumericVector weights, IntegerVector idx_global, IntegerVector idx_local) {
  int tmp_idx = which_min(weights);
  IntegerVector min_idx = { tmp_idx };
  IntegerVector supp_idx = idx_global[min_idx];
  IntegerVector supp_idx_local = idx_local[min_idx];

  int res_global = supp_idx[0];
  int res_local = supp_idx_local[0];
  IntegerVector result = {res_global, res_local};
  return(result);
}

List constraint_info(CharacterVector sdc, IntegerVector freqs, NumericVector weights, IntegerVector indices) {
  int nr_primsupps = 0;
  int nr_secondsupps = 0;
  int nr_singletons = 0;

  int avail_s = 0;
  int avail_z = 0;
  int avail_w = 0;
  int amount_supped = 0;

  // "indices relevant to global, overall position */
  IntegerVector ind_poss_s, ind_poss_z, ind_poss_w, ind_poss_s_or_z, ind_primsupps;

  // possible indices relevant to local position */
  IntegerVector local_poss_primsupps, local_poss_s, local_poss_s_or_z, local_poss_z, local_poss_w;

  for (int i = 0; i < sdc.size(); i++) {
    if (sdc[i] == "u") {
      amount_supped = amount_supped + freqs[i];
      nr_primsupps = nr_primsupps + 1;
      if (freqs[i] == 1) {
        nr_singletons = nr_singletons + 1;
      }
      ind_primsupps.push_back(indices[i]);
      local_poss_primsupps.push_back(i);
    }
    if (sdc[i] == "x") {
      amount_supped = amount_supped + freqs[i];
      nr_secondsupps = nr_secondsupps + 1;
      if (freqs[i] == 1) {
        nr_singletons = nr_singletons + 1;
      }
    }
    if (sdc[i] == "s") {
      ind_poss_s.push_back(indices[i]);
      local_poss_s.push_back(i);
      ind_poss_s_or_z.push_back(indices[i]);
      local_poss_s_or_z.push_back(i);
      avail_s = avail_s + freqs[i];
    }
    if (sdc[i] == "z") {
      ind_poss_z.push_back(indices[i]);
      local_poss_z.push_back(i);
      ind_poss_s_or_z.push_back(indices[i]);
      local_poss_s_or_z.push_back(i);
      avail_z = avail_z + freqs[i];
    }
    if (sdc[i] == "w") {
      ind_poss_w.push_back(indices[i]);
      local_poss_w.push_back(i);
      amount_supped = amount_supped + freqs[i];
      avail_w = avail_w + freqs[i];
    }
  }

  // returns various information about current sub-problem, relating to a specific constraint
  IntegerVector v_nr = rep(0, 9);
  v_nr.names() = CharacterVector({"s", "w", "z", "sz", "primsupps", "secondsupps", "non_w", "singletons", "supps"});
  v_nr["s"] = ind_poss_s.size();
  v_nr["z"] = ind_poss_z.size();
  v_nr["w"] = ind_poss_w.size();
  v_nr["sz"] = ind_poss_s_or_z.size();
  v_nr["primsupps"] = nr_primsupps;
  v_nr["secondsupps"] = nr_secondsupps;
  int nr_non_w = indices.size() - v_nr["w"];
  v_nr["non_w"] = nr_non_w;
  v_nr["singletons"] = nr_singletons;
  v_nr["supps"] = nr_primsupps + nr_secondsupps;

  IntegerVector v_amount = IntegerVector::create(avail_s, avail_z, avail_w, amount_supped);
  v_amount.names() = CharacterVector({"avail_s", "avail_z", "avail_w", "supped"});

  List poss_ind = List::create(
    Named("primsupps") = ind_primsupps,
    Named("s") = ind_poss_s,
    Named("z") = ind_poss_z,
    Named("w") = ind_poss_w,
    Named("s_or_z") = ind_poss_s_or_z
  );

  List local_ind = List::create(
    Named("primsupps") = local_poss_primsupps,
    Named("s") = local_poss_s,
    Named("z") = local_poss_z,
    Named("w") = local_poss_w,
    Named("s_or_z") = local_poss_s_or_z
  );

  bool fully_supped = v_nr["supps"] == nr_non_w;
  return Rcpp::List::create(
    Named("idx") = indices,
    Named("freqs") = freqs,
    Named("sdc") = sdc,
    Named("weights") = weights,
    Named("fully_supped") = fully_supped,
    Named("numbers") = v_nr,
    Named("poss_ind") = poss_ind,
    Named("local_ind") = local_ind,
    Named("amounts") = v_amount
  );
}

/* computes sdc-constraints for each constraint in the input list */
List sdc_info(List inp) {
  IntegerVector freqs = inp["freqs"];
  NumericVector weights = inp["weights"];

  CharacterVector sdc_status = inp["sdc_status"];
  int nr_constraints = inp["nr_constraints"];

  List con = inp["constraints"];
  List out(nr_constraints); //pre-allocate

  for (int i = 0; i < nr_constraints; i++) {
    //Rcout << "dealing with constraint " << i << std::endl;

    List cur_constraint = con[i];
    IntegerVector idx = cur_constraint["idx"];
    IntegerVector f = freqs[idx];
    NumericVector w = weights[idx];
    CharacterVector s = sdc_status[idx];
    out[i] = constraint_info(s, f, w, idx);
  }
  return(out);
}

/*
 * Inputs:
 * - con: one of the constraints retrieved via constraint_info() or sdc_info()
 * - sdc: the full sdc-status vector
 * - do_singletons: if true, we try to identify additional required suppressions due to singletons
 * - threshold: threshold-value used to identify additional required suppressions (if > 0)
 * - run: if > 1, singleton-detection procedure is not required to be run (again)
 */
List supp_constraint(List con, CharacterVector sdc, bool do_singletons, double threshold, int run) {
  /*
   * makes sure that for each constraint at least 2 cells are suppressed
   * if do_singletons == true: possibly additional suppressions are done if one ore two primary singleton suppressions are part of the pattern
   * if threshold > 0: additional cells are suppressed until the threshold is reached or the row is fully_suppressed
   */
  Function cpp_print("print");
  IntegerVector idx, cur_freq, numbers, poss_s, poss_idx;
  CharacterVector cur_sdc, old_sdc;
  NumericVector cur_w, amounts, poss_w;
  int nr_singletons, nr_supps, idx_global, idx_local;
  IntegerVector additional_supps;
  bool fully_supped = con["fully_supped"];

  // we cannot do anything more if all cells are already suppressed
  if (fully_supped == true) {
    return(Rcpp::List::create(
        Named("additional_supps") = additional_supps,
        Named("con") = con
    ));
  }

  idx = con["idx"];
  cur_sdc = sdc[idx];
  old_sdc = con["sdc"];
  cur_freq = con["freqs"];
  numbers = con["numbers"];
  amounts = con["amounts"];
  cur_w = con["weights"];

  int nr_primsupps = numbers["primsupps"];
  List poss_indices_local = con["local_ind"];
  List poss_indices_global = con["poss_ind"];
  bool recheck_measures = true;

  if (run == 1 and (do_singletons == true or threshold > 0)) {
    nr_singletons = numbers["singletons"];
    if (nr_singletons == 0 ) {
      // no singletons in this constraint -> nothing todo
      do_singletons = false;
    }

    if (do_singletons == true) {
      nr_supps = numbers["supps"];
      //Rcout << "nr_supps: " << nr_supps  << " | nr_singletons: " << nr_singletons << std::endl;
      // first case: we have a constraint with 2 primary suppressions,
      // one of them being a singleton; we need an additional suppression
      if (nr_supps == 2 and nr_singletons >= 1) {
        poss_s = poss_indices_local["s"];
        poss_w = cur_w[poss_s];
        poss_idx = idx[poss_s];

        IntegerVector add_supp = find_additional_suppression(poss_w, poss_idx, poss_s);
        idx_global = add_supp[0];
        idx_local = add_supp[1];

        //Rcout << "additional suppression found: " << idx_global << " (" << idx_local << ")" << std::endl;

        // in order to return the value
        additional_supps.push_back(idx_global);

        // recompute sdc-measures for current constraint
        cur_sdc[idx_local] = "x";
        List con2 = constraint_info(cur_sdc, con["freqs"], con["weights"], idx);
        con = con2;

        // we need to update measures that might have changed
        numbers = con["numbers"];
        nr_singletons = numbers["singletons"];
        nr_supps = numbers["supps"];
        poss_indices_local = con["local_ind"];
        poss_indices_global = con["poss_ind"];
        fully_supped = con["fully_supped"];
        recheck_measures = false;
      }

      // if a frequency rule is used, it could happen that two cells on a row/column are
      // primary unsafe, but the sum of the two cells could still be unsafe. In that case
      // it should be prevented that these two cells protect each other.
      if ((cur_sdc[0] == "u") and (nr_supps == 3) and (nr_primsupps == 3) and (fully_supped == false)) {
        poss_s = poss_indices_local["s"];
        poss_w = cur_w[poss_s];
        poss_idx = idx[poss_s];

        IntegerVector add_supp = find_additional_suppression(poss_w, poss_idx, poss_s);
        idx_global = add_supp[0];
        idx_local = add_supp[1];

        //Rcout << "additional suppression found: " << idx_global << " (" << idx_local << ")" << std::endl;

        // in order to return the value
        additional_supps.push_back(idx_global);

        // recompute sdc-measures for current constraint
        cur_sdc[idx_local] = "x";
        List con2 = constraint_info(cur_sdc, con["freqs"], con["weights"], idx);
        con = con2;

        // we need to update measures that might have changed
        numbers = con["numbers"];
        nr_singletons = numbers["singletons"];
        nr_supps = numbers["supps"];
        poss_indices_local = con["local_ind"];
        poss_indices_global = con["poss_ind"];
        fully_supped = con["fully_supped"];
        recheck_measures = false;
      }
    }

    amounts = con["amounts"];
    double amount_supped = amounts["supped"];
    if (threshold > 0 and amount_supped < threshold) {
      //Rcout << "--> threshold: " << threshold << " --> we suppress cells until threshold is reached " << std::endl;
      fully_supped = con["fully_supped"];
      while ((amount_supped < threshold) and (fully_supped == false)) {
        poss_s = poss_indices_local["s"];
        poss_w = cur_w[poss_s];
        poss_idx = idx[poss_s];

        IntegerVector add_supp = find_additional_suppression(poss_w, poss_idx, poss_s);
        idx_global = add_supp[0];
        idx_local = add_supp[1];
        //Rcout << "additional suppression (threshold) found: " << idx_global << " (" << idx_local << ")" << std::endl;

        // in order to return the value
        additional_supps.push_back(idx_global);

        // recompute sdc-measures for current constraint
        cur_sdc[idx_local] = "x";
        List con2 = constraint_info(cur_sdc, con["freqs"], con["weights"], idx);
        con2["sdc"] = cur_sdc;
        con = con2;

        // we need to update measures that might have changed
        numbers = con["numbers"];
        nr_singletons = numbers["singletons"];
        nr_supps = numbers["supps"];
        poss_indices_local = con["local_ind"];
        poss_indices_global = con["poss_ind"];
        amounts = con["amounts"];
        fully_supped = con["fully_supped"];
        amount_supped = amounts["supped"];
      }
      recheck_measures = false; // we have just recomputed the sdc-measures
    }
  }

  if (fully_supped == true) {
    return(Rcpp::List::create(
        Named("additional_supps") = additional_supps,
        Named("con") = con
    ));
  }

  if (recheck_measures == true) {
    bool all_equal = compare_charvecs(old_sdc, cur_sdc);
    if (all_equal == false) {
      // the sdc-pattern has changed; we need to re-compute the
      // sdc-measures for the given constraint
      List con2 = constraint_info(cur_sdc, con["freqs"], con["weights"], idx);
      con2["sdc"] = cur_sdc;
      con = con2;
    }
  }

  numbers = con["numbers"];
  amounts = con["amounts"];
  nr_supps = numbers["supps"];
  double amount_w = amounts["avail_w"];
  fully_supped = con["fully_supped"];
  poss_indices_local = con["local_ind"];

  // this is the default case; we have a single suppression in the
  // constraint which is not fully suppressed and the amount of noice
  // suppressed by "w"-cells (not published) is also zero
  if ((nr_supps == 1) and (amount_w <= 0) and (fully_supped == false)) {
    //Rcout << "default case: find a single additional suppression" << std::endl;
    poss_s = poss_indices_local["s"];
    poss_w = cur_w[poss_s];
    poss_idx = idx[poss_s];

    IntegerVector add_supp = find_additional_suppression(poss_w, poss_idx, poss_s);
    idx_global = add_supp[0];
    idx_local = add_supp[1];
    //Rcout << "additional suppression (default-case) found: " << idx_global << " (" << idx_local << ")" << std::endl;

    // append value to additional cells to suppress
    additional_supps.push_back(idx_global);

    cur_sdc[idx_local] = "x";
    List con2 = constraint_info(cur_sdc, con["freqs"], con["weights"], idx);
    con2["sdc"] = cur_sdc;
    con = con2;
    List xx = Rcpp::List::create(
      Named("additional_supps") = additional_supps,
      Named("con") = con
    );
  }
  return(Rcpp::List::create(
      Named("additional_supps") = additional_supps,
      Named("con") = con
  ));
}

// [[Rcpp::export]]
List suppConstraints(DataFrame dat, List m, List params) {
  Function cpp_print("print");

  LogicalVector v_is_common = dat["is_common_cell"];
  IntegerVector v_freqs = dat["freq"];
  NumericVector v_weights = dat["weight.for.suppression"];
  CharacterVector sdc = dat["sdcStatus"];
  CharacterVector v_sdc = clone(sdc);

  /* params */
  bool param_verbose = params["verbose"];
  bool param_check_constraints = params["check_constraints"];
  bool param_do_singletons = params["do_singletons"];
  double param_threshold = params["threshold"];

  bool check_for_overlaps = is_true(any(v_is_common));
  List constraints = create_supp_inputs(m, v_is_common, v_freqs, v_weights, v_sdc, check_for_overlaps, param_verbose);

  if (param_check_constraints == true) {
    if (param_verbose == true) {
      Rcout << "checking constraint validity" << std::endl;
    }
    bool all_ok = check_constraints(constraints);
    if (all_ok == false) {
      stop("invalid constraints detected.");
    }
  }

  List constraint_info = sdc_info(constraints);
  IntegerVector new_supps;
  List tmpres;
  List cur_constraint, result;
  int run = 0;
  bool finished = false;
  bool additional_supps;
  int nr_add_supps = 0;
  while (finished == false) {
    run = run + 1;
    additional_supps = false;
    nr_add_supps = 0;
    if (param_verbose == true) {
      Rcout << "run: " << run;
    }

    for (int i = 0; i < constraint_info.size(); i++) {
      cur_constraint = constraint_info[i];
      tmpres = supp_constraint(cur_constraint, v_sdc, param_do_singletons, param_threshold, run);
      new_supps = tmpres["additional_supps"];
      constraint_info[i] =  tmpres["con"]; // possibly newly computed measures
      nr_add_supps = nr_add_supps + new_supps.size();
      if (new_supps.size() > 0) {
        v_sdc[new_supps] = "x";
        additional_supps = true;
      }
    }
    if (param_verbose == true) {
      Rcout << " | new suppressions: " << nr_add_supps << std::endl;
    }
    if (additional_supps == false) {
      finished = true;
    }
  }

  return(List::create(
      Named("sdc_status") = v_sdc,
      Named("constraints") = constraints
  ));
}
