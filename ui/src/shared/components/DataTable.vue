<template>
	<div>
		<el-row>
			<el-col :span="24">
				<el-button @click="httpAddItem()">Add</el-button>
			</el-col>
		</el-row>
		<el-row>
			<el-col :span="24">
				<el-table :data="tableData.items">
					<el-table-column label="" fixed>
						<template slot-scope="scope">
							<el-button @click="httpUpdateItem(scope.$index)">Update</el-button>
							<el-button @click="httpRemoveItem(scope.$index)">Delete</el-button>
						</template>
					</el-table-column>
					<slot></slot>
				</el-table>
			</el-col>
		</el-row>
		<el-row>
			<el-col :span="8" :offset="8">
				<el-pagination
					@current-change="handleCurrentChange"
					:current-page.sync="curPage"
					:page-size="count"
					layout="total, prev, pager, next, jumper"
					:page-count="total"
					>
				</el-pagination>
			</el-col>
		</el-row>
	</div>
</template>

<script>
	import { HTTP, httpWithNotify } from '@/shared/http-common.js'
import moment from 'moment'

export default {
	props: ['url','itemUrl','tdata'],
	data() {
		return{
			count: 20,
			curPage: 0,
			offset: 0,
			total: 0,
			tableData: this.tdata
		}
	},
	created() {
		this.httpGetData();
		httpWithNotify(
			'',
			"Couldn't get status",
			HTTP.get(this.url+"/size"),
			true
		).then(d => {
			this.total = Math.ceil(d/this.count);
		});
	},
	methods:{
		httpGetData(){
			let q = this.url;
			let params = {
				offset: this.offset,
				count: this.count
			};
			httpWithNotify(
				'',
				'Could not get expenses',
				HTTP.get(q, { params }),
				true
			).then(d => {
				this.tableData = d;
				this.emitChange();
			});
		},
		updateData() {
			let q = this.url;
			let t = this.tableData;
			httpWithNotify(
				'Updated!',
				'Could not update',
				HTTP.put(q, t)
			).then(x => {
				this.emitChange();
			});
		},
		httpUpdateItem(index){
			let item = this.tableData.items[index];
			let q = this.url + "/" + this.itemUrl +"/id/" + item.id;
			let i = item;
			httpWithNotify(
				'Updated Item',
				'Could not update item',
				HTTP.put(q,i)
			).then(x => {
				this.emitChange();
			});
		},
		httpRemoveItem(index){
			let item  = this.tableData.items[index];
			let q = this.url + "/" + this.itemUrl +"/id/" + item.id;
			httpWithNotify(
				'Item deleted',
				'Could not delete item' ,
				HTTP.delete(q)
			).then(x => {
				this.httpGetData();
				this.emitChange();
			});
		},
		httpAddItem(){
			let q = this.url + "/" + this.itemUrl;
			let newItem = {
				name: '',
				amount: 0,
				reason: '',
				type: '',
				id: '',
				date: {
					tag: "OneTime",
					contents: moment().format()
				},
				rate: {
					tag: "OneTime",
					contents: moment().format()
				}
			};

			httpWithNotify(
				'Added Item!',
				'Could not add item',
				HTTP.post(q,newItem)
			).then(x => {
				this.httpGetData();
				this.emitChange();
			});
		},
		handleCurrentChange(){
			this.offset = (this.curPage-1)*this.count;
			this.httpGetData();
		},
		emitChange(){
			this.$emit('update:tdata', this.tableData);
		}
	}
}
</script>
