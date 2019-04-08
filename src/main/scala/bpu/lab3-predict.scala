package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 0)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{
  /********* new attempt *********/

  class SendInfo extends Bundle {
    val pc = UInt(10.W)
    val branch_history  = UInt(10.W)
    val prediction_count = UInt(1.W)
    val prediction = UInt(1.W)
  }

  // init vectors
  val local_hist_table = Reg(
    init = Vec(Seq.fill(1024) { UInt("b0000000000", width = 10) }))
  val local_count_table = Reg(
    init = Vec(Seq.fill(1024) { UInt("b00", width = 2) }))
  val global_hist_count_table = Reg(
    init = Vec(Seq.fill(1024) { UInt("b00", width = 2) }))
  val pred_hist_count_table = Reg(
    init = Vec(Seq.fill(1024) { UInt("b00", width = 2) }))

  // pause prediction
  val stall = !io.resp.ready

  // get values EVERY CLOCK CYCLE
  val pc = io.req_pc
  val pc_index = pc >> UInt(log2Ceil(coreInstBytes))
  val pc_hist = RegEnable(local_hist_table(pc_index), !stall) // L1 history table: pc -> 10 bit histry
  val pc_count = RegEnable(local_count_table(pc_hist), !stall) // L1 count table: hist -> count
  
  val branch_hist = io.resp.bits.history.get // may need to change
  val global_count = RegEnable(global_hist_count_table(branch_hist), !stall)
  val pred_count = RegEnable(pred_hist_count_table(branch_hist), !stall)

  val pred = Mux(pred_count(1),
                  pc_count(1),
                  global_count(1))

  val s_info = Wire(new SendInfo)
  s_info.pc := pc_index
  s_info.branch_history := branch_hist
  s_info.prediction_count := pred_count(1)
  s_info.prediction := pred          

  // send prediction
  io.resp.valid := !this.disable_bpd
  io.resp.bits.takens := pred
  // tell the pipeline to save the index for commit
  io.resp.bits.info := RegNext(s_info.asUInt()) 

  val info_commit = (new SendInfo).fromBits(this.commit.bits.info.info)

  val commit_en = this.commit.valid
  val commit_info_pc = info_commit.pc 
  val commit_info_hist = info_commit.branch_history 
  val commit_info_pred_choice = info_commit.prediction_count 
  val commit_info_pred = info_commit.prediction 
  val commit_taken = this.commit.bits.ctrl.taken(0)

  // index into table to get previous states
  val commit_pc_hist = RegEnable(local_hist_table(commit_info_pc), !stall) // L1 history table: pc -> 10 bit histry
  val commit_pc_count = RegEnable(local_count_table(commit_pc_hist), !stall) // L1 count table: hist -> count
  
  val commit_global_count = RegEnable(global_hist_count_table(commit_info_hist), !stall)
  val commit_pred_count = RegEnable(pred_hist_count_table(commit_info_hist), !stall)

  // calculate and update values
  val commit_local_hist_update =  Mux(commit_taken,
                                        (commit_pc_hist << 1) + 1.U,
                                        commit_pc_hist << 1)
  when (commit_en) { local_hist_table(commit_info_pc) := commit_local_hist_update } // use old pc to index
 
  val commit_local_count_update =  Mux(commit_taken,
                                      Mux(commit_pc_count === "b11".U, commit_pc_count, commit_pc_count + 1.U),
                                      Mux(commit_pc_count === "b00".U, commit_pc_count, commit_pc_count - 1.U))
  when (commit_en) { local_count_table(commit_pc_hist) := commit_local_count_update } // use old branch specific histry to index

  val commit_global_count_update =  Mux(commit_taken,
                                      Mux(commit_global_count === "b11".U, commit_global_count, commit_global_count + 1.U),
                                      Mux(commit_global_count === "b00".U, commit_global_count, commit_global_count - 1.U))
  when (commit_en) { global_hist_count_table(commit_info_hist) := commit_global_count_update } 

  // choice 1 implies 1 on pred counter
  val pred_incorrect = Mux(commit_info_pred_choice === "b1".U, 
                             Mux(commit_pred_count === "b00".U, 
                                          commit_pred_count, 
                                          commit_pred_count - 1.U), 
                             Mux(commit_pred_count === "b11".U, 
                                          commit_pred_count, 
                                          commit_pred_count + 1.U))

  val pred_correct = Mux(commit_info_pred_choice === "b1".U, 
                             Mux(commit_pred_count === "b11".U, 
                                          commit_pred_count, 
                                          commit_pred_count + 1.U), 
                             Mux(commit_pred_count === "b00".U, 
                                          commit_pred_count, 
                                          commit_pred_count - 1.U))

  val commit_pred_count_update =  Mux(commit_info_pred === commit_taken,
                                      pred_correct, 
                                      pred_incorrect) 
  when (commit_en) { pred_hist_count_table(commit_info_hist) := commit_pred_count_update }

}

