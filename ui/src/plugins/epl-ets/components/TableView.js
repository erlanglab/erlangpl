// @flow
import React from 'react';

import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
import {
  Table,
  Column,
  SortDirection
} from 'react-virtualized/dist/commonjs/Table';
import 'react-virtualized/styles.css';
import './Table.css';

import Select from 'react-select';
import 'react-select/dist/react-select.css';

class TableView extends React.Component {
  state: any;
  constructor(props: any) {
    super(props);
    this.state = {
      sortBy: 'name',
      sortDirection: SortDirection.ASC,
      selectLabel: 'Select columns to show',
      selectOptions: [
        { value: 'id', label: 'ID' },
        { value: 'memory', label: 'Memory' },
        { value: 'size', label: 'Size' },
        { value: 'type', label: 'Type' },
        { value: 'write_concurrency', label: 'Write concurrency' },
        { value: 'read_concurrency', label: 'Read concurrency' },
        { value: 'lookup_max', label: 'lookup max time' },
        { value: 'lookup_count', label: 'lookup count' },
        { value: 'insert_max', label: 'insert max time' },
        { value: 'insert_count', label: 'insert count' },
        { value: 'owner', label: 'Owner' },
        { value: 'heir', label: 'Heir' },
        { value: 'protection', label: 'Protection' }
      ],
      selectedOptions: [
        { value: 'memory', label: 'Memory' },
        { value: 'size', label: 'Size' },
        { value: 'type', label: 'Type' },
        { value: 'write_concurrency', label: 'Write concurrency' },
        { value: 'read_concurrency', label: 'Read concurrency' },
        { value: 'lookup_max', label: 'lookup max time' },
        { value: 'lookup_count', label: 'lookup count' },
        { value: 'insert_max', label: 'insert max time' },
        { value: 'insert_count', label: 'insert count' }
      ]
    };
  }

  isSelected(val: string) {
    let listOfObjects = this.state.selectedOptions.filter(({ value }) => {
      return value === val;
    });
    if (listOfObjects.length > 0) {
      return true;
    }
    return false;
  }

  propComparatorASC(prop: string) {
    if (prop === 'name') {
      return function(a: string, b: string) {
        var nameA = a[prop].toLowerCase(),
          nameB = b[prop].toLowerCase();
        if (nameA < nameB) return -1;
        return 1;
      };
    }
    return (a: any, b: any) => a[prop] - b[prop];
  }

  propComparatorDESC(prop: string) {
    if (prop === 'name') {
      return function(a: string, b: string) {
        var nameA = a[prop].toLowerCase(),
          nameB = b[prop].toLowerCase();
        if (nameA > nameB) return -1;
        return 1;
      };
    }
    return (a: any, b: any) => b[prop] - a[prop];
  }

  sort(listOfObjects: Array<*>) {
    if (this.state.sortDirection === SortDirection.ASC) {
      return listOfObjects.sort(this.propComparatorASC(this.state.sortBy));
    } else {
      return listOfObjects.sort(this.propComparatorDESC(this.state.sortBy));
    }
  }

  render() {
    let tabData = this.props.table.tabs.filter(function(node) {
      let newName = this.split('.').join('_').replace('@', '_at_');
      return node.name === newName;
    }, this.props.table.node);
    if (tabData[0].length < 1) return null;
    const list = tabData[0].tabs.map(function({
      info,
      call_stats,
      tab_id,
      tab_trace_id
    }) {
      let callStatsLookup = {
        lookupMax: 0,
        lookupCount: 0
      };
      let callStatsInsert = {
        insertMax: 0,
        insertCount: 0
      };
      if (call_stats !== 'undefined' && typeof call_stats !== 'undefined') {
        let lookup = call_stats.filter(function(obj) {
          return obj.func === 'lookup';
        });
        let insert = call_stats.filter(function(obj) {
          return obj.func === 'insert';
        });
        if (lookup.length !== 0) {
          callStatsLookup.lookupMax = lookup[0].max_time;
          callStatsLookup.lookupCount = lookup[0].count;
        }
        if (insert.length !== 0) {
          callStatsInsert.insertMax = insert[0].max_time;
          callStatsInsert.insertCount = insert[0].count;
        }
      }
      let callStatsObj = {
        ...callStatsInsert,
        ...callStatsLookup
      };
      return {
        tab_id,
        tab_trace_id,
        ...info,
        ...callStatsObj
      };
    });
    const listSorted = this.state.sortBy ? this.sort(list) : list;
    const rowGetter = ({ index }) => listSorted[index];
    return (
      <AutoSizer>
        {({ width, height }) =>
          <div>
            <div>
              <h4 className="selection">
                {this.state.selectLabel}
              </h4>
              <Select
                name="size-select"
                multi={true}
                onChange={selected => {
                  this.setState({ selectedOptions: selected });
                }}
                options={this.state.selectOptions}
                value={this.state.selectedOptions}
                autosize={true}
                className={'ets-table-select'}
                optionClassName={'ets-table-select-opts'}
              />
            </div>
            <Table
              ref="Table"
              disableHeader={false}
              width={width}
              headerHeight={60}
              height={height - 160}
              rowHeight={30}
              rowGetter={rowGetter}
              rowCount={list.length}
              sortBy={this.state.sortBy}
              sortDirection={this.state.sortDirection}
              onRowDoubleClick={({ rowData }) => {
                this.props.tableClicked({
                  tabId: rowData.tab_id,
                  tabTraceId: rowData.tab_trace_id
                });
              }}
              rowClassName={({ index }) => {
                if (index !== -1) {
                  return 'ets-table-row';
                } else {
                  return 'ets-table-header';
                }
              }}
              sort={({ sortBy, sortDirection }) => {
                this.setState({ sortBy, sortDirection });
              }}
            >
              <Column
                width={150}
                label="Name"
                cellRenderer={({ cellData }) => cellData}
                dataKey="name"
                disableSort={false}
              />
              {this.isSelected('id') &&
                <Column
                  width={150}
                  label="ID"
                  cellRenderer={({ cellData }) => cellData}
                  dataKey="tab_id"
                  disableSort={true}
                />}
              {this.isSelected('memory') &&
                <Column
                  width={100}
                  label="Memory"
                  dataKey="memory"
                  cellRenderer={({ cellData }) => cellData}
                  disableSort={false}
                />}
              {this.isSelected('size') &&
                <Column
                  width={100}
                  label="Size"
                  dataKey="size"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('type') &&
                <Column
                  width={100}
                  label="Type"
                  disableSort
                  dataKey="type"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('write_concurrency') &&
                <Column
                  width={100}
                  label="Write concurrency"
                  disableSort
                  dataKey="write_concurrency"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('read_concurrency') &&
                <Column
                  width={100}
                  label="Read concurrency"
                  disableSort
                  dataKey="read_concurrency"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('lookup_max') &&
                <Column
                  width={100}
                  label="lookup max time [ms]"
                  dataKey="lookupMax"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('lookup_count') &&
                <Column
                  width={100}
                  label="lookup count"
                  dataKey="lookupCount"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('insert_max') &&
                <Column
                  width={100}
                  label="insert max time [ms]"
                  dataKey="insertMax"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('insert_count') &&
                <Column
                  width={100}
                  label="insert count"
                  dataKey="insertCount"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('owner') &&
                <Column
                  width={100}
                  label="Owner"
                  disableSort
                  dataKey="owner"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('heir') &&
                <Column
                  width={100}
                  label="Heir"
                  disableSort
                  dataKey="heir"
                  cellRenderer={({ cellData }) => cellData}
                />}
              {this.isSelected('protection') &&
                <Column
                  width={100}
                  label="Protection"
                  disableSort
                  dataKey="protection"
                  cellRenderer={({ cellData }) => cellData}
                />}
            </Table>
          </div>}
      </AutoSizer>
    );
  }
}

export default TableView;
