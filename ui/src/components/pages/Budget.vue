<template>
	<el-row>
		<el-row><h1>Budget: {{ budgetName }}</h1></el-row>
		<el-row :gutter="20">
			<el-col :span="12">
				<budgets-graph :budget-name="budgetName"></budgets-graph>
			</el-col>
			<el-col :span="12">
				<el-row>
					<h2>Start Info</h2>
					<el-form :inline="true" v-for="(item, index) in startInfo" :key="index">
						<el-form-item label="Start Amount">
							<el-input v-model.number="item.startAmount" type="number" placeholder="Amount"></el-input>
						</el-form-item>
						<el-form-item label="Start Date">
							<el-date-picker
								v-model="item.startDate"
								type="date"
								format="yyyy-MM-dd"
								placeholder="Budget Start Day">
							</el-date-picker>
						</el-form-item>
					</el-form>
					<h2>Budget Items</h2>
					<el-form :inline="true" v-for="(item, index) in budgetData" :key="index">
						<el-form-item label="Name">
							<el-input v-model="item.type" placeholder="Item Type"></el-input>
						</el-form-item>
						<el-form-item label="Amount">
							<el-input v-model.number="item.amount" type="number" placeholder="Item Amount"></el-input>
						</el-form-item>
						<el-form-item label="Rate">
							<el-input v-model.number="item.rate" type="number" placeholder="Item Rate"></el-input>
						</el-form-item>
					</el-form>
				</el-row>
			</el-col>
		</el-row>
	</el-row>
</template>

<script>
	import Vue from 'vue'
import BudgetsGraph from '@/shared/components/BudgetsGraph.vue'
import statusJSON from '@/assets/budget-status.json'
import _ from 'lodash'
import { HTTP, baseURL } from '@/shared/http-common'
import moment from 'moment'

export default {
	components: {
		BudgetsGraph
	},
	data () {
		return {
			budgetData: null,
			budgetName: this.$route.params.name,
			startInfo: null
		}
	},
	created () {
		this.httpGetBudget();
	},
	methods: {
		httpGetBudget(){
			let sdate = moment().subtract(30, 'days').format("YYYY-MM-DD");
			let edate = moment().format("YYYY-MM-DD");
			let query = baseURL+"budget/budget-by-name/" + this.budgetName;

			HTTP.get(query)
				.then(response => {
					this.budgetData = response.data.items;
					this.startInfo = response.data.startInfo;
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
						message: 'Could not get expenses at: ' + query,
						duration: 0
					})
				});
		}
	}
}
</script>
