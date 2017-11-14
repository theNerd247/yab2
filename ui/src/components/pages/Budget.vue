<template>
	<el-row>
		<el-row>
			<el-col :span="22">
				<h1>Budget: {{ budgetName }}</h1>
			</el-col>

			<el-col :span="2">
				<router-link tag="el-button" :to="{name: 'Expenses', params: {name: this.$route.params.name}}">Go To Expenses</router-link>
			</el-col>
		</el-row>

		<el-row :gutter="20">
			<el-col :span="12">
				<budgets-graph :budget-name="budgetName"></budgets-graph>
			</el-col>

			<el-col :span="12">
				<el-row>
					<h2>Start Info</h2>
					<el-form :inline="true" v-if="budgetData != null">
						<el-form-item label="Start Amount">
							<el-input v-model.number="budgetData.startInfo.startAmount" type="number" placeholder="Amount"></el-input>
						</el-form-item>
						<el-form-item label="Start Date">
							<el-date-picker
								v-model="budgetData.startInfo.startDate"
								type="date"
								format="yyyy-MM-dd"
								placeholder="Budget Start Day">
							</el-date-picker>
						</el-form-item>
					</el-form>
				</el-row>

				<el-row>
					<h2>Budget Items</h2>
					<el-button @click="httpUpdateBudget">Update Budget</el-button>
					<DataTable :url="url" :itemUrl="itemUrl" :tdata.sync="budgetData">
						<el-table-column label="Type">
							<template slot-scope="scope">
								<el-input v-model="scope.row.type" placeholder="Item Type"></el-input>
								</template>
							</el-table-column>
						<el-table-column label="Amount">
							<template slot-scope="scope">
								<el-input v-model.number="scope.row.amount" type="number" placeholder="Item Amount"></el-input>
								</template>
							</el-table-column>
						<el-table-column label="Rate">
							<template slot-scope="scope">
								<rate :rate.sync="scope.row.rate"></rate>
								</template>
							</el-table-column>
						</DataTable>
				</el-row>
			</el-col>
		</el-row>
	</el-row>
</template>

<script>
	import Vue from 'vue'
import BudgetsGraph from '@/shared/components/BudgetsGraph.vue'
import Rate from '@/shared/components/Rate.vue'
import DataTable from '@/shared/components/DataTable.vue'
import statusJSON from '@/assets/budget-status.json'
import _ from 'lodash'
import { HTTP, httpWithNotify } from '@/shared/http-common'
import moment from 'moment'

export default {
	components: {
		BudgetsGraph,
		Rate,
		DataTable
	},
	data () {
		return {
			url: "budget-list/name/" + this.$route.params.name,
			itemUrl: "budget",
			budgetData: null,
			budgetName: this.$route.params.name,
		}
	},
	methods: {
		httpUpdateBudget(){
			httpWithNotify(
				'Updated Budget', 
				"Could not update budget", 
				HTTP.put(this.url, this.budgetData)
			);
		}
	}
}
</script>
