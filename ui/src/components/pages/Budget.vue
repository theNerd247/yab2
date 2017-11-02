<template>
	<el-row>
		<el-row>
      <el-col :span="22">
      <h1>Budget: {{ budgetName }}</h1>
      </el-col>
      <el-col :span="2">
        <el-button @click=updateBudget()>Update Budget</el-button>
      </el-col>
    </el-row>

		<el-row :gutter="20">
			<el-col :span="12">
				<budgets-graph :budget-name="budgetName"></budgets-graph>
			</el-col>
			<el-col :span="12">
				<el-row>
					<h2>Start Info</h2>
					<el-form :inline="true">
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
					<h2>Budget Items</h2>
					<el-form :inline="true" v-for="(item, index) in budgetData.items" :key="index">
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
			query: "/budget-list/name/" + this.$route.params.name
		}
	},
	created () {
		this.httpGetBudget();
	},
	methods: {
		httpGetBudget(){
			let sdate = moment().subtract(30, 'days').format();
			let edate = moment().format();

			HTTP.get(this.query)
				.then(response => {
					this.budgetData = response.data;
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
						message: 'Could not get expenses at: ' + query,
						duration: 0
					})
				});
		},
    updateBudget() {
      HTTP.put(this.query, this.budgetData)
      .then(response => {
        this.$notify({
          title: 'Updated Budget',
          type: 'success',
          duration: 0
        })
      })
      .catch(e => {
        this.$notify.error({
          title: 'Error',
          message: 'Could not update budget:\n' + JSON.stringify(e.response.data),
          duration: 0
        })
      });

    }
	}
}
</script>
