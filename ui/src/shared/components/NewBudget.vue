<template>
	<home-card title="New Budget">
		<el-button slot="action" @click="createNewBudget" type="info" style="float: right;"><i class="el-icon-plus"></i> Add</el-button>
		<el-form slot="content">
			<el-form-item>
				<el-input v-model="newBudget.startInfo.name" placeholder="Budget Name"></el-input>
			</el-form-item>
			<el-form-item>
				<el-input v-model.number="newBudget.startInfo.startAmount" type="number" placeholder="Start Amount"></el-input>
			</el-form-item>
			<el-form-item>
				<el-date-picker
					v-model="newBudget.startInfo.startDate"
					type="date"
					format="yyyy-MM-dd"
					placeholder="Budget Start Day">
				</el-date-picker>
			</el-form-item>
		</el-form>
	</home-card>
</template>

<script>
	import Vue from 'vue'
import HomeCard from './HomeCard.vue'
import budgetsJSON from '@/assets/budgets.json'
import _ from 'lodash'
import { HTTP, httpWithNotify } from '@/shared/http-common'
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
					id: '',
					startAmount: 0,
					startDate: moment(),
				},
				items: []
			}
		}
	},
	methods: {
		createNewBudget () {
			_.remove(this.newBudget.items, x => x.amount === null || x.rate == null || x.rate < 0)

			this.newBudget.startInfo.startDate = moment(this.newBudget.startDate).format();
			this.newBudget.items = _.map(this.newBudget.items, x => {x.name = this.newBudget.name; return x});

			httpWithNotify(
				'Created new budget',
				'Could not create budget',
				HTTP.post('budget-list', this.newBudget)
			);
		},
	}
}
</script>
