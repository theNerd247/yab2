<style>
  .el-row {
    margin-bottom: 20px;
  }
  .el-table .alert-row {
    background: #FF4949;
  }
</style>

<template>
  <el-col :span="24">
    <el-row type="flex" justify="space-around" :gutter="20">
      <el-col :span="8">
          <add-expense></add-expense>
      </el-col>

      <el-col :span="8">
        <el-row>
          <new-budget></new-budget>
        </el-row>
        <el-row>
          <upload-transaction></upload-transaction>
        </el-row>
      </el-col>

      <el-col :span="8">
        <el-row>
          <status-chart></status-chart>
        </el-row>
      </el-col>
    </el-row>
  </el-col>
</template>

<script>
import Vue from 'vue'
import BudgetGraph from '@/shared/components/BudgetsGraph.vue'
import StatusChart from '@/shared/components/StatusChart.vue'
import UploadTransaction from '@/shared/components/UploadTransaction.vue'
import AddExpense from '@/shared/components/AddExpense.vue'
import RecentExpenses from '@/shared/components/RecentExpenses.vue'
import NewBudget from '@/shared/components/NewBudget.vue'
import { HTTP } from '@/shared/http-common.js'

export default {
  components: {
    BudgetGraph,
    StatusChart,
    UploadTransaction,
    AddExpense,
    NewBudget
  },
  data () {
    return {
      budgetNames: []
    }
  },
  name: 'Vue',

}
</script>

