<template>
	<home-card title="New Budget">
		<el-button slot="action" @click="createNewBudget" type="info" style="float: right;"><i class="el-icon-plus"></i> Add</el-button>
		<el-form slot="content">
			<el-form-item>
				<el-input v-model="newBudget.name" placeholder="Budget Name"></el-input>
			</el-form-item>
			<el-form-item>
				<el-input v-model.number="newBudget.startAmount" type="number" placeholder="Start Amount"></el-input>
			</el-form-item>
      <el-form-item>
           <el-date-picker
            v-model="newBudget.startDate"
            type="date"
            format="yyyy-MM-dd"
            placeholder="Budget Start Day">
          </el-date-picker>
      </el-form-item>
      <el-form-item>
          <el-button @click="addItem()"><i class="el-icon-plus"></i> Add Item</el-button>
      </el-form-item>
			<el-form-item v-for="(item, index) in newBudget.items" :key="index">
        <el-input v-model="item.type" placeholder="Item Type">
          <el-button slot="prepend" @click="removeItem(index)"><i class="el-icon-circle-cross"></i></el-button>
        </el-input>
				<el-input v-model.number="item.amount" type="number" placeholder="Item Amount"></el-input>
				<el-input v-model.number="item.rate" type="number" placeholder="Item Rate"></el-input>
			</el-form-item>
		</el-form>
	</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from './HomeCard.vue'
import budgetsJSON from '@/assets/budgets.json'
import _ from 'lodash'
import { HTTP, baseURL } from '@/shared/http-common'
import moment from 'moment'

export default {
	components: {
		HomeCard,
	},
	data  () {
		return {
			newBudget: {
        startInfo: {
          name: '',
          id: 0,
          startAmount: null,
          startDate: null,
          name: '',
        },
        items: [ {id: 0, type: '', amount: null, rate: null, name: '' } ]
			}
		}
	},
	methods: {
    createNewBudget () { 
      _.remove(this.newBudget.items, x => x.amount === null || x.rate == null || x.rate < 0)

      this.newBudget.startInfo.startDate = moment(this.newBudget.startDate).format();
      this.newBudget.items = _.map(this.newBudget.items, x => {x.name = this.newBudget.name; return x});

      console.log(this.newBudget);
      HTTP.post('budget', this.newBudget)
      .then(response => {
        this.$notify({
          title: 'Created new budget',
          message: this.newBudget.name,
          type: 'success',
          duration: 0
        })
      })
      .catch(e => {
        this.$notify.error({
          title: 'Error',
          message: 'Could not create new budget',
          duration: 0
        })
      });

    },
    removeItem(index) {
      this.newBudget.items = this.newBudget.items.splice(index,1);
    },
    addItem(){
      this.newBudget.items.push( {id: 0, type: '', amount: null, rate: null, name: '' });
    }
	}
}
</script>
