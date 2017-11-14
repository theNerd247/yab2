<template>
  <el-row>
    <el-row>
      <el-col :span="22">
        <h1>{{ budgetName }}</h1>
      </el-col>

      <el-col :span="2">
        <router-link tag="el-button" :to="{name: 'Budget', params: {name: this.$route.params.name}}">Go To Budget</router-link>
      </el-col>
    </el-row>
    <el-row>
      <DataTable :url="url" :itemUrl="itemUrl" :tdata.sync="expensesData">
        <el-table-column label="Date">
          <template slot-scope="scope">
            <el-date-picker
              v-model="scope.row.date.contents"
              type="date"
              format="yyyy-MM-dd"
              placeholder="Date">
            </el-date-picker>
          </template>
        </el-table-column>
        <el-table-column label="Budget">
          <template slot-scope="scope">
            <el-input v-model="scope.row.name" placeholder="Budget Name"></el-input>
          </template>
        </el-table-column>
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
        <el-table-column label="Reason">
          <template slot-scope="scope">
            <el-input v-model="scope.row.reason" placeholder="Reason"></el-input>
          </template>
        </el-table-column>
      </DataTable>
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
import DataTable from '@/shared/components/DataTable.vue'

export default {
  components: {
    DataTable
  },
  data () {
    return {
      budgetName: this.$route.params.name,
      expensesData: null,
			url: "expense-list/name/" + this.$route.params.name,
			itemUrl: "expense"
    }
  },
}
</script>
