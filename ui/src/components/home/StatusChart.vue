<template>
<home-card title="Current Balances">
		<el-table slot="content" :data="statuses" :row-class-name="budgetStatus">
			<el-table-column prop="name" label="Budget" ></el-table-column>
			<el-table-column prop="amount" label="Amount" > </el-table-column>
			<el-table-column prop="limit" label="Limit"> </el-table-column>
	</el-table>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from '@/shared/HomeCard.vue'
import statusJSON from '@/assets/budget-status.json'
import _ from 'lodash'

export default {
  components: {
    HomeCard,
	},
	data  () {
		return {
		}
	},
	computed: {
		statuses () {
			return _.take(statusJSON, 10);
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
