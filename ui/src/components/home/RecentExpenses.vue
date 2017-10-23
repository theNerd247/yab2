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
import { HTTP } from '@/shared/http-common'
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
			let sdate = moment().subtract(30, 'days').format("YYYY-MM-DD");
			let edate = moment().format("YYYY-MM-DD");
			let query = "expenses/by-date/" + sdate + "/" + edate;

			HTTP.get(query)
				.then(response => {
          console.log(response.data.length);
					this.expenses = response.data;
          this.expenses = _.reverse(this.expenses);
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
						message: 'Could not get expenses at: ' + query
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
