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
  constructor(props) {
    super(props);
    this.state = {
      sortBy: '',
      sortDirection: SortDirection.ASC,
      selectLabel: 'Select columns to show',
      selectOptions: [
        { value: 'name', label: 'name' },
        { value: 'size', label: 'Size' },
        { value: 'memory', label: 'Memory' },
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
        { value: 'name', label: 'Name' },
        { value: 'size', label: 'Size' },
        { value: 'memory', label: 'Memory' },
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

  is_selected(val) {
    var listOfObjects = this.state.selectedOptions.filter(({ value, ...r }) => {
      return value === val;
    });
    if (listOfObjects.length > 0) {
      return true;
    }
    return false;
  }

  propComparatorASC(prop) {
    return function(a, b) {
      return a[prop] - b[prop];
    };
  }

  propComparatorDESC(prop) {
    return function(a, b) {
      return b[prop] - a[prop];
    };
  }

  sort(listOfObjects) {
    if (this.state.sortDirection === SortDirection.ASC) {
      return listOfObjects.sort(this.propComparatorASC(this.state.sortBy));
    } else {
      return listOfObjects.sort(this.propComparatorDESC(this.state.sortBy));
    }
  }

  render() {
    var tabData = this.props.table.tabs.filter(function(node) {
      var newName = this.split('.').join('_').replace('@', '_at_');
      return node.name === newName;
    }, this.props.table.node);
    if (tabData[0].length < 1) return null;
    const list = tabData[0].tabs.map(function({ info, callStats, ...a }) {
      var callStatsLookup = {
        lookupMax: 0,
        lookupCount: 0
      };
      var callStatsInsert = {
        insertMax: 0,
        insertCount: 0
      };
      if (callStats !== 'undefined' && typeof callStats !== 'undefined') {
        var lookup = callStats.filter(function(obj) {
          return obj.func === 'lookup';
        });
        var insert = callStats.filter(function(obj) {
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
      var callStatsObj = {
        ...callStatsInsert,
        ...callStatsLookup
      };
      return { ...a, ...info, ...callStatsObj };
    });

    const listSorted = this.state.sortBy ? this.sort(list) : list;
    const rowGetter = ({ index }) => listSorted[index];
    return (
      <AutoSizer>
        {({ width, height }) => (
          <div>
            <div>
              <h4 className="selection">{this.state.selectLabel}</h4>
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
              {this.is_selected('name')
                ? <Column
                    width={150}
                    label="Name"
                    cellRenderer={({ cellData }) => cellData}
                    dataKey="name"
                    disableSort={true}
                  />
                : null}
              {this.is_selected('memory')
                ? <Column
                    width={100}
                    label="Memory"
                    dataKey="memory"
                    cellRenderer={({ cellData }) => cellData}
                    disableSort={false}
                  />
                : null}
              {this.is_selected('size')
                ? <Column
                    width={100}
                    label="Size"
                    dataKey="size"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('type')
                ? <Column
                    width={100}
                    label="Type"
                    disableSort
                    dataKey="type"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('write_concurrency')
                ? <Column
                    width={100}
                    label="Write concurrency"
                    disableSort
                    dataKey="write_concurrency"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('read_concurrency')
                ? <Column
                    width={100}
                    label="Read concurrency"
                    disableSort
                    dataKey="read_concurrency"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_max')
                ? <Column
                    width={100}
                    label="lookup max time"
                    dataKey="lookupMax"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_count')
                ? <Column
                    width={100}
                    label="lookup count"
                    dataKey="lookupCount"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_max')
                ? <Column
                    width={100}
                    label="insert max time"
                    dataKey="insertMax"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_count')
                ? <Column
                    width={100}
                    label="insert count"
                    dataKey="insertCount"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('owner')
                ? <Column
                    width={100}
                    label="Owner"
                    disableSort
                    dataKey="owner"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('heir')
                ? <Column
                    width={100}
                    label="Heir"
                    disableSort
                    dataKey="heir"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('protection')
                ? <Column
                    width={100}
                    label="Protection"
                    disableSort
                    dataKey="protection"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
            </Table>
          </div>
        )}
      </AutoSizer>
    );
  }
}

export default TableView;
