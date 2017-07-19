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
      sortBy: 'index',
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
        { value: 'protection', label: 'Protection' },
        { value: 'lookup_min', label: 'lookup min time' },
        { value: 'lookup_median', label: 'lookup 50th time' },
        { value: 'lookup_percentile_75', label: 'lookup 75th time' },
        { value: 'lookup_percentile_90', label: 'lookup 90th time' },
        { value: 'lookup_percentile_95', label: 'lookup 95th time' },
        { value: 'lookup_percentile_99', label: 'lookup 99th time' },
        { value: 'lookup_percentile_999', label: 'lookup 999th time' },
        { value: 'insert_min', label: 'insert min time' },
        { value: 'insert_median', label: 'insert 50th time' },
        { value: 'insert_percentile_75', label: 'insert 75th time' },
        { value: 'insert_percentile_90', label: 'insert 90th time' },
        { value: 'insert_percentile_95', label: 'insert 95th time' },
        { value: 'insert_percentile_99', label: 'insert 99th time' },
        { value: 'insert_percentile_999', label: 'insert 999th time' }
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

  render() {
    var TabData = this.props.table.tabs.filter(function(node) {
      var NewName = this.split('.').join('_').replace('@', '_at_');
      return node.name === NewName;
    }, this.props.table.node);
    if (TabData[0].length < 1) return null;
    const list = TabData[0].tabs.map(function({ info, call_stats, ...a }) {
      var call_stats_lookup = {
        lookup_max: 0,
        lookup_min: 0,
        lookup_median: 0,
        lookup_percentile_75: 0,
        lookup_percentile_90: 0,
        lookup_percentile_95: 0,
        lookup_percentile_99: 0,
        lookup_percentile_999: 0,
        lookup_count: 0
      };
      var call_stats_insert = {
        insert_max: 0,
        insert_min: 0,
        insert_median: 0,
        insert_percentile_75: 0,
        insert_percentile_90: 0,
        insert_percentile_95: 0,
        insert_percentile_99: 0,
        insert_percentile_999: 0,
        insert_count: 0
      };
      if (call_stats !== 'undefined' && typeof call_stats !== 'undefined') {
        var Lookup = call_stats.filter(function(obj) {
          return obj.func === 'lookup';
        });
        var Insert = call_stats.filter(function(obj) {
          return obj.func === 'insert';
        });
        if (Lookup.length !== 0) {
          call_stats_lookup.lookup_max = Lookup[0].time.max;
          call_stats_lookup.lookup_min = Lookup[0].time.min;
          call_stats_lookup.lookup_median = Lookup[0].time.median;
          call_stats_lookup.lookup_percentile_75 =
            Lookup[0].time.percentile[75];
          call_stats_lookup.lookup_percentile_90 =
            Lookup[0].time.percentile[90];
          call_stats_lookup.lookup_percentile_95 =
            Lookup[0].time.percentile[95];
          call_stats_lookup.lookup_percentile_99 =
            Lookup[0].time.percentile[99];
          call_stats_lookup.lookup_percentile_999 =
            Lookup[0].time.percentile[999];
          call_stats_lookup.lookup_count = Lookup[0].count;
        }
        if (Insert.length !== 0) {
          call_stats_insert.insert_max = Insert[0].time.max;
          call_stats_insert.insert_min = Insert[0].time.min;
          call_stats_insert.insert_median = Insert[0].time.median;
          call_stats_insert.insert_percentile_75 =
            Insert[0].time.percentile[75];
          call_stats_insert.insert_percentile_90 =
            Insert[0].time.percentile[90];
          call_stats_insert.insert_percentile_95 =
            Insert[0].time.percentile[95];
          call_stats_insert.insert_percentile_99 =
            Insert[0].time.percentile[99];
          call_stats_insert.insert_percentile_999 =
            Insert[0].time.percentile[999];
          call_stats_insert.insert_count = Insert[0].count;
        }
      }
      var call_stats_obj = {
        ...call_stats_insert,
        ...call_stats_lookup
      };
      return { ...a, ...info, ...call_stats_obj };
    });
    const rowGetter = ({ index }) => list[index];
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
              height={height}
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
                    disableSort={false}
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
                    dataKey="lookup_max"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_count')
                ? <Column
                    width={100}
                    label="lookup count"
                    dataKey="lookup_count"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_max')
                ? <Column
                    width={100}
                    label="insert max time"
                    dataKey="insert_max"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_count')
                ? <Column
                    width={100}
                    label="insert count"
                    dataKey="insert_count"
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
              {this.is_selected('lookup_min')
                ? <Column
                    width={100}
                    label="lookup min time"
                    dataKey="lookup_min"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_median')
                ? <Column
                    width={100}
                    label="lookup median time"
                    dataKey="lookup_median"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_percentile_75')
                ? <Column
                    width={100}
                    label="lookup 75th percentile time"
                    dataKey="lookup_percentile_75"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_percentile_90')
                ? <Column
                    width={100}
                    label="lookup 90 percentile time"
                    dataKey="lookup_percentile_90"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_percentile_95')
                ? <Column
                    width={100}
                    label="lookup 95 percentile time"
                    dataKey="lookup_percentile_95"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_percentile_99')
                ? <Column
                    width={100}
                    label="lookup 99 percentile time"
                    dataKey="lookup_percentile_99"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('lookup_percentile_999')
                ? <Column
                    width={100}
                    label="lookup 999 percentile time"
                    dataKey="lookup_percentile_999"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_min')
                ? <Column
                    width={100}
                    label="insert min time"
                    dataKey="insert_min"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_median')
                ? <Column
                    width={100}
                    label="insert median time"
                    dataKey="insert_median"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_percentile_75')
                ? <Column
                    width={100}
                    label="insert 75th percentile time"
                    dataKey="insert_percentile_75"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_percentile_90')
                ? <Column
                    width={100}
                    label="insert 90 percentile time"
                    dataKey="insert_percentile_90"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_percentile_95')
                ? <Column
                    width={100}
                    label="insert 95 percentile time"
                    dataKey="insert_percentile_95"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_percentile_99')
                ? <Column
                    width={100}
                    label="insert 99 percentile time"
                    dataKey="insert_percentile_99"
                    cellRenderer={({ cellData }) => cellData}
                  />
                : null}
              {this.is_selected('insert_percentile_999')
                ? <Column
                    width={100}
                    label="insert 999 percentile time"
                    dataKey="insert_percentile_999"
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
