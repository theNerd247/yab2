<style>
  .el-row {
    margin-bottom: 20px;
  }
  .el-table .alert-row {
    background: #FF4949;
  }
</style>
<template>
  <el-row :gutter="20">
    <el-col :span="24">
    <el-row>
      <el-menu theme="dark" :default-active="activeIndex" class="el-menu-demo" mode="horizontal" @select="handleSelect">
        <el-menu-item index="1">YAB</el-menu-item>
      </el-menu>
    </el-row>

    <el-row type="flex" justify="space-around" :gutter="20">
      <el-col :span="8">
				<budget-graph></budget-graph>
			</el-col>

      <el-col :span="8">
				<el-row>
					<add-expense></add-expense>
				</el-row>
				<el-row>
					<upload-transaction></upload-transaction>
				</el-row>
			</el-col>

			<el-col :span="8">
				<status-chart></status-chart>
      </el-col>
    </el-row>

    <el-row :gutter="20" justify="space-around">
      <el-col :span="8">
      </el-col>

      <el-col :span="8">
      </el-col>

      <el-col :span="8">
      </el-col>
    </el-row>

    </el-col>
  </el-row>
</template>

<script>
import Vue from 'vue'
import BudgetGraph from '@/components/home/BudgetsGraph.vue'
import StatusChart from '@/components/home/StatusChart.vue'
import UploadTransaction from '@/components/home/UploadTransaction.vue'
import AddExpense from '@/components/home/AddExpense.vue'
import HomeCard from '@/shared/HomeCard.vue'

export default {
  components: {
    HomeCard,
		BudgetGraph,
		StatusChart,
		UploadTransaction,
		AddExpense,
  },
  name: 'Vue',
  data () {
    return {
    }
  },
}
</script>

