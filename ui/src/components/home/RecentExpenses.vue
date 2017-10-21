<template>
<home-card title="Recent Expenses">
		<el-table slot="content" :data="recentExpenses">
			<el-table-column prop="name" label="Budget" ></el-table-column>
			<el-table-column prop="date" label="Date" ></el-table-column>
			<el-table-column prop="amount" label="Amount" > </el-table-column>
			<el-table-column prop="reason" label="Reason"> 
				<template slot-scope="props">
					{{ makeReason(props.row.reason) }}
				</template>
			</el-table-column>
	</el-table>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from '@/shared/HomeCard.vue'
import expensesJSON from '@/assets/expenses.json'
import _ from 'lodash'

export default {
  components: {
    HomeCard,
	},
	methods: {
		makeReason(reason) {
			return _.truncate(reason,{length: 15, separator: ' '});
		}
	},
	computed: {
		recentExpenses () {
			return _.take(expensesJSON,10);
		}
	}
}
</script>
