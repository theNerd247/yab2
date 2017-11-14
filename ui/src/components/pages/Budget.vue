<style>
	.el-row--flex {
		flex-direction: row;
	}
	.full {
		height: 100vh;
	}
</style>
<template>
	<el-container>
		<el-header height="max-content">
			<el-row>
				<el-col :span="12">
					<h1>Budget: {{ budgetName }}</h1>
				</el-col>
			</el-row>
			<el-row :gutter="20">
				<el-col :span="12">
					<h2>Status</h2>
					<StatusRange :status="status"></StatusRange>
				</el-col>
				<el-col :span="12">
					<budgets-graph :budget-name="budgetName"></budgets-graph>
				</el-col>
			</el-row>
		</el-header>

		<el-main>
			<el-row type="flex">
				<el-col class="full">
					<el-row>
						<el-col>
							<h2>Start Info</h2>
							<el-form :inline="true" v-if="budgetData != null">
								<el-form-item>
									<el-button @click="httpUpdateBudget">Update Budget</el-button>
								</el-form-item>
								<el-form-item label="Start Amount">
									<el-input size="small" v-model.number="budgetData.startInfo.startAmount" type="number" placeholder="Amount">
									</el-input>
								</el-form-item>
								<el-form-item label="Start Date">
									<el-date-picker size="small" v-model="budgetData.startInfo.startDate" type="date" format="yyyy-MM-dd" placeholder="Budget Start Day"> </el-date-picker>
								</el-form-item>
							</el-form>
						</el-col>
					</el-row>
					<el-row>
						<h2>Budget Items</h2>
						<DataTable :url="burl" :itemUrl="bitemUrl" :tdata.sync="budgetData">
							<el-table-column label="Type">
								<template slot-scope="scope">
									<el-input size="small" v-model="scope.row.type" placeholder="Item Type"></el-input>
								</template>
							</el-table-column>
							<el-table-column label="Amount">
								<template slot-scope="scope">
									<el-input size="small" v-model.number="scope.row.amount" type="number" placeholder="Item Amount"></el-input>
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

			<el-row type="flex">
				<el-col class="full">
					<h2>Expenses</h2>
					<DataTable :url="eurl" :itemUrl="eitemUrl" :tdata.sync="expensesData">
						<el-table-column label="Date">
							<template slot-scope="scope">
								<el-date-picker size="small" v-model="scope.row.date.contents" type="date" format="yyyy-MM-dd" placeholder="Date"> </el-date-picker>
							</template>
						</el-table-column>
						<el-table-column label="Budget">
							<template slot-scope="scope">
								<el-input size="small" v-model="scope.row.name" placeholder="Budget Name"></el-input>
							</template>
						</el-table-column>
						<el-table-column label="Type">
							<template slot-scope="scope">
								<el-input size="small" v-model="scope.row.type" placeholder="Item Type"></el-input>
							</template>
						</el-table-column>
						<el-table-column label="Amount">
							<template slot-scope="scope">
								<el-input size="small" v-model.number="scope.row.amount" type="number" placeholder="Item Amount"></el-input>
							</template>
						</el-table-column>
						<el-table-column label="Reason">
							<template slot-scope="scope">
								<el-input size="small" v-model="scope.row.reason" placeholder="Reason"></el-input>
							</template>
						</el-table-column>
					</DataTable>
				</el-col>
			</el-row>

		</el-main>
	</el-container>
</template>

<script>
	import Vue from 'vue'
import BudgetsGraph from '@/shared/components/BudgetsGraph.vue'
import Rate from '@/shared/components/Rate.vue'
import DataTable from '@/shared/components/DataTable.vue'
import statusJSON from '@/assets/budget-status.json'
import StatusRange from '@/shared/components/StatusRange.vue'
import _ from 'lodash'
import { HTTP, httpWithNotify } from '@/shared/http-common'
import moment from 'moment'

export default {
	components: {
		BudgetsGraph,
		Rate,
		DataTable,
		StatusRange
	},
	data () {
		return {
			burl: "budget-list/name/" + this.$route.params.name,
			bitemUrl: "budget",
			budgetData: {items: [], startInfo: {startAmount: 0, startDate: ""}},
			budgetName: this.$route.params.name,
			expensesData: {items: [], startInfo: {startAmount: 0, startDate: ""}},
			eurl: "expense-list/name/" + this.$route.params.name,
			eitemUrl: "expense",
			status: [0,0]
		}
	},
	created () {
		this.httpGetStatus();
	},
	watch: {
		budgetData () {
			this.httpGetStatus();
		},
		expensesData () {
			this.httpGetStatus();
		}
	},
	methods: {
		httpGetStatus() {
			httpWithNotify(
				'',
				'Could not get budget status',
				HTTP.get(this.burl+"/status/on/"+moment().format("YYYY-MM-DD")),
				true
			).then(d => {
				this.status = d
			});
		},
		httpUpdateBudget(){
			httpWithNotify(
				'Updated Budget',
				"Could not update budget",
				HTTP.put(this.burl, this.budgetData)
			).then(d =>{
				this.budgetData = d;
			});
		}
	}
}
</script>
