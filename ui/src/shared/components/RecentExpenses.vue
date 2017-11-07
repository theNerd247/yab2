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
import HomeCard from './HomeCard.vue'
import expensesJSON from '@/assets/expenses.json'
import _ from 'lodash'
import { HTTP, baseURL } from '@/shared/http-common'
import moment from 'moment'

export default {
  components: {
    HomeCard,
	},
	data () {
		return {
			expenses: []
		}
	},
	created () {
		this.httpGetExpenses();
	},
	methods: {
		makeReason(reason) {
			return _.truncate(reason,{length: 15, separator: ' '});
		},
		httpGetExpenses(){
			let query = "expenses"

			HTTP.get(query)
				.then(response => {
					this.expenses = response.data.items;
          this.expenses = _.reverse(this.expenses);
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
            message: 'Could not get expenses at: ' + query,
            duration: 0
					})
				});
		}
	},
	computed: {
		recentExpenses () {
			return _.take(this.expenses,10);
		}
	}
}
</script>
