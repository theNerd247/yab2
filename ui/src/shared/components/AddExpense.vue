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
        <el-date-picker
          v-model="newExpense.date.contents"
          type="date"
          format="yyyy-MM-dd"
          placeholder="Date">
        </el-date-picker>
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
import { HTTP, httpWithNotify } from '@/shared/http-common.js'


export default {
  components: {
    HomeCard,
    BudgetNames,
  },
  data  () {
    return {
      newExpense: {
        name: '',
        amount: 0,
        reason: '',
        type: '',
        id: '',
        date: {
          tag: "OneTime",
          contents: moment().format()
        }
      }
    }
  },
  methods: {
    createNewExpense () {
      let query = "expense-list/name/"+this.newExpense.name+"/expense";
      let exp = this.newExpense;
      httpWithNotify(
        'Created New Expense',
        'Could not create expense',
        HTTP.post(query,exp));
    },
  }
}
</script>
