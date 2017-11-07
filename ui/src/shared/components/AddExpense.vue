<template>
	<home-card title="Add Expense">
		<el-button slot="action" @click="createNewExpense" type="info" style="float: right;"><i class="el-icon-plus"></i> Add</el-button>
		<el-form slot="content">
			<el-form-item>
        <BudgetNames :budgetName.sync="newExpense.name"></BudgetNames>
      </el-form-item>
			<el-form-item>
				<el-input v-model="newExpense.reason" placeholder="Reason"></el-input>
			</el-form-item>
			<el-form-item>
				<el-input v-model="newExpense.date" placeholder="Date"></el-input>
			</el-form-item>
      <el-form-item>
        <el-input v-model="newExpense.type" placeholder="Budget Item Name"></el-input>
      </el-form-item>
			<el-form-item>
				<el-input v-model.number="newExpense.amount" type="number" placeholder="Amount">
					<span slot="prepend">$</span>
				</el-input>
			</el-form-item>
		</el-form>
	</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from './HomeCard.vue'
import BudgetNames from './BudgetNames.vue'
import budgetsJSON from '@/assets/budgets.json'
import moment from 'moment'
import { HTTP } from '@/shared/http-common.js'


export default {
	components: {
		HomeCard,
    BudgetNames,
	},
	data  () {
		return {
			expenses: [],
			newExpense: {
				name: '',
				amount: 0,
				reason: '',
        type: '',
        id: '',
        date: moment().format("YYYY-MM-DD")
			}
		}
	},
	methods: {
		createNewExpense () { 
      let query = "expense-list/name/"+this.newExpense.name+"/expense"; 
			this.newExpense.date = moment(this.newExpense.date).format();
      HTTP.post(query,this.newExpense)
      .then(response => {
        this.$notify({
          title: 'Created new expense',
          message: response.data.id,
          type: 'success',
          duration: 0
        })
      })
      .catch(e => {
        this.$notify.error({
          title: 'Error',
          message: 'Could not create new expense:\n' + JSON.stringify(e.response.data),
          duration: 0
        })
      });
    },
	}
}
</script>
