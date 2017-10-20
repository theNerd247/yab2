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

    <el-row :gutter="20">
      <el-col :span="8">
        <home-card title="Budget Status">
          <div slot="content">
            <balances-chart ref="bchart" :chart-data="balancesData"></balances-chart>
          </div>
        </home-card>
      </el-col>

      <el-col :span="8">
        <home-card title="Add Expense"> <el-button slot="action" @click="createNewExpense" style="float: right;">New</el-button> <el-form slot="content"> <el-form-item> <el-input v-model="newExpense.name" placeholder="Budget Name"></el-input> </el-form-item> <el-form-item> <el-input v-model="newExpense.reason" placeholder="Reason"></el-input> </el-form-item> <el-form-item> <el-input v-model="newExpense.amount" placeholder="Amount"> <span slot="prepend">$</span> </el-input> </el-form-item> </el-form> </home-card> </el-col> <el-col :span="8"> <home-card title="Upload Transactions"> <el-button slot="action" @click="uploadTransaction" style="float: right;">Upload</el-button> <el-form slot="content"> <el-form-item> <el-input placeholder="Budget Name"></el-input> </el-form-item> <el-form-item> <el-upload class="upload-demo" drag action="https://jsonplaceholder.typicode.com/posts/" multiple> <i class="el-icon-upload"></i> <div class="el-upload__text">Drop file here or <em>click to upload</em></div> </el-upload> </el-form-item>
          </el-form>
        </home-card>
      </el-col>
    </el-row>
    <el-row :gutter="20">
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
import HomeCard from './HomeCard.vue'
import budgetsJSON from '../assets/budgets.json'
import BalancesChart from '../shared/BalancesChart.vue'

export default {
  components: {
    HomeCard,
    BalancesChart
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
			balancesData: null,
      gradient1: null,
			gradient2: null
    }
  },
  mounted () {
		let ys = [];
		let xs = [];
		let is = [];
		for (var i = 0; i <= 62.8; i++){
			is[i] = i;
			xs[i] = 0.2*Math.cos(0.1*i);
			ys[i] = Math.sin(0.1*i);
		};

		this.gradient1 =
			this.$refs.bchart.$refs.canvas.getContext('2d').createLinearGradient(0, 0, 0, 1000);
		this.gradient1.addColorStop(0, 'rgba(19, 206, 102, 0.5)')
		this.gradient1.addColorStop(0.5, 'rgba(19, 206, 102, 0.25)');
		this.gradient1.addColorStop(1, 'rgba(19, 206, 102, 0)');

		this.gradient2 =
			this.$refs.bchart.$refs.canvas.getContext('2d').createLinearGradient(0, 0, 0, 1000);
		this.gradient2.addColorStop(0, 'rgba(32, 160,255, 1)')
		this.gradient2.addColorStop(0.5, 'rgba(32, 160, 255, 0)');
		this.gradient2.addColorStop(1, 'rgba(32, 160, 255, 1)');


		this.balancesData = {
			labels: is,
			datasets: [
				{
					label: 'Budget',
					backgroundColor: this.gradient2,
					data: ys
				},
				{
					label: 'Expenses',
					backgroundColor: this.gradient1,
					data: xs 
				}
			]
		};    
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

