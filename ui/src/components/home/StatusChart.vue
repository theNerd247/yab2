<template>
<home-card title="Current Balances">
		<el-table slot="content" :data="budgets" :row-class-name="budgetStatus">
			<el-table-column prop="name" label="Budget" ></el-table-column>
			<el-table-column prop="amount" label="Amount" > </el-table-column>
			<el-table-column prop="limit" label="Limit"> </el-table-column>
	</el-table>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from '@/shared/HomeCard.vue'
import budgetsJSON from '@/assets/budgets.json'

export default {
  components: {
    HomeCard,
	},
	data  () {
		return {
      budgets: budgetsJSON,
		}
	},
  methods: {
    budgetStatus(budget) {
      if(budget.amount > budget.limit)
        return 'alert-row';
      else
        return '';
    }
  }
}
</script>
