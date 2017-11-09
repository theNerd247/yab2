<template>
<el-row>
  <el-row>
    <h1>Budget: {{ budgetName }}</h1>
  </el-row>

  <el-row>
    <h2>Expenses</h2>
    <el-button @click="updateExpenses()">Update Expenses</el-button>
    <el-button @click="addExpensesItem()">Add Item</el-button>
    <el-table :data="expensesData.items" height="100%">
      <el-table-column label="" fixed>
        <template slot-scope="scope">
            <el-button @click="removeExpensesItem(scope.$index)">Update</el-button>
            <el-button @click="updateExpenseItem(scope.$index)">Delete</el-button>
        </template>
      </el-table-column>
      <el-table-column label="Type">
        <template slot-scope="scope">
          <el-input v-model="scope.row.type" placeholder="Item Type"></el-input>
        </template>
      </el-table-column>
      <el-table-column label="Date">
        <template slot-scope="scope">
          <el-input v-model="scope.row.date" placeholder="Date">
            <el-button slot="prepend" @click="removeExpensesItem(scope.$index)">Delete</el-button>
          </el-input>
        </template>
      </el-table-column>
      <el-table-column label="Amount">
        <template slot-scope="scope">
          <el-input v-model.number="scope.row.amount" type="number" placeholder="Item Amount"></el-input>
        </template>
      </el-table-column>
      <el-table-column label="Reason">
        <template slot-scope="scope">
          <el-input v-model="scope.row.reason" placeholder="Reason"></el-input>
        </template>
      </el-table-column>
    </el-table>
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
  data () {
    return {
      budgetName: this.$route.params.name,
      expensesData: null,
    }
  },  
  created() {
    this.httpGetExpenses();
  },
  methods: {
    httpGetExpenses(){
      HTTP.get("/expense-list/name/" + this.$route.params.name)
        .then(response => {
          this.expensesData = response.data;
        })
        .catch(e => {
          this.$notify.error({
            title: 'Error',
            message: 'Could not get expenses at: ' + query,
            duration: 0
          })
        });
    },
    updateExpenses () {
      HTTP.put("/expense-list/name/" + this.$route.params.name, this.expensesData)
        .then(response => {
          this.$notify({
            title: 'Updated Expenses',
            type: 'success',
            duration: 0
          });

          this.httpGetExpenses();
        })
        .catch(e => {
          this.$notify.error({
            title: 'Error',
            message: 'Could not update expenses:\n' + JSON.stringify(e.response.data),
            duration: 0
          })
        });
    },
    removeExpensesItem(index) {
      this.expensesData.items.splice(index,1);
    },
    addExpensesItem(){
      this.expensesData.items.push( {id: "", type: '', amount: null, rate: null, name: this.expensesData.startInfo.name });
    }
  }
}
</script>
