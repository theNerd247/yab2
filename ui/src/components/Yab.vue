<style>
  .el-row {
    margin-bottom: 20px;
  }
  .el-table .alert-row {
    background: #FF4949;
  }
</style>
<template>
  <el-row :gutter="20">
    <el-col :span="24">
    <el-row>
      <el-menu theme="dark" :default-active="activeIndex" class="el-menu-demo" mode="horizontal" @select="handleSelect">
        <el-menu-item index="1">YAB</el-menu-item>
      </el-menu>
    </el-row>

    <el-row type="flex" justify="space-around" :gutter="20">
      <el-col :span="8">
				<budget-graph></budget-graph>
			</el-col>

      <el-col :span="8">
				<home-card title="Add Expense"> 
					<el-button slot="action" @click="createNewExpense" type="info" style="float: right;"><i class="el-icon-plus"></i> Add</el-button> 
					<el-form slot="content"> 
						<el-form-item> 
							<el-input v-model="newExpense.name" placeholder="Budget Name"></el-input>
						</el-form-item>
						<el-form-item>
								<el-input v-model="newExpense.reason" placeholder="Reason"></el-input>
						</el-form-item>
						<el-form-item>
								<el-input v-model="newExpense.amount" placeholder="Amount">
								<span slot="prepend">$</span>
								</el-input>
						</el-form-item>
						</el-form>
						</home-card>
			</el-col>
			<el-col :span="8">
				<home-card title="Upload Transactions">
					<el-button slot="action" @click="uploadTransaction" type="info" style="float: right;"><i class="el-icon-upload"></i> Upload</el-button>
					<el-form slot="content">
					<el-form-item>
						<el-input placeholder="Budget Name"></el-input>
					</el-form-item>
					<el-form-item>
						<el-col :span="16" :offset="4">
							<el-upload class="upload-demo" drag action="https://jsonplaceholder.typicode.com/posts/" multiple>
								<i class="el-icon-upload"></i>
									<div class="el-upload__text">Drop file here or <em>click to upload</em></div>
								</el-upload>
							</el-col>
						</el-form-item>
          </el-form>
        </home-card>
      </el-col>
    </el-row>
    <el-row :gutter="20" justify="space-around">
      <el-col :span="8">
        <home-card title="Current Balances">
            <el-table slot="content" :data="budgets" :row-class-name="budgetStatus">
              <el-table-column prop="name" label="Budget" ></el-table-column>
              <el-table-column prop="amount" label="Amount" > </el-table-column>
              <el-table-column prop="limit" label="Limit"> </el-table-column>
          </el-table>
        </home-card>
      </el-col>

      <el-col :span="8">
        <home-card title="Recent Expenses">
        </home-card>
      </el-col>

      <el-col :span="8">
        <home-card></home-card>
      </el-col>
    </el-row>

    </el-col>
  </el-row>
</template>

<script>
import Vue from 'vue'
import BudgetGraph from '@/components/home/BudgetsGraph.vue'
import budgetsJSON from '../assets/budgets.json'
import HomeCard from '@/shared/HomeCard.vue'

export default {
  components: {
    HomeCard,
		BudgetGraph
  },
  name: 'Vue',
  data () {
    return {
      budgets: budgetsJSON,
      newExpense: {
        name: '',
        amount: null,
        reason: ''
      },
    }
  },
  methods: {
    budgetStatus(budget) {
      if(budget.amount > budget.limit)
        return 'alert-row';
      else
        return '';
    }
  }
}
</script>

