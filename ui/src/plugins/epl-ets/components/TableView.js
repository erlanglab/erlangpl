// @flow
import React from 'react';

import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
import { Table, Column } from 'react-virtualized/dist/commonjs/Table';
import 'react-virtualized/styles.css';

const TableView = ({ table }) => {
  if (table.length < 1) return null;
  //const list = table[0].tabs.map(({ info, ...a }) => ({ ...a, ...info }));
  const list = table[0].tabs.map(function({ info, call_stats, ...a }) {
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
        call_stats_lookup.lookup_percentile_75 = Lookup[0].time.percentile[75];
        call_stats_lookup.lookup_percentile_90 = Lookup[0].time.percentile[90];
        call_stats_lookup.lookup_percentile_95 = Lookup[0].time.percentile[95];
        call_stats_lookup.lookup_percentile_99 = Lookup[0].time.percentile[99];
        call_stats_lookup.lookup_percentile_999 =
          Lookup[0].time.percentile[999];
        call_stats_lookup.lookup_count = Lookup[0].count;
      }
      if (Insert.length !== 0) {
        call_stats_insert.insert_max = Insert[0].time.max;
        call_stats_insert.insert_min = Insert[0].time.min;
        call_stats_insert.insert_median = Insert[0].time.median;
        call_stats_insert.insert_percentile_75 = Insert[0].time.percentile[75];
        call_stats_insert.insert_percentile_90 = Insert[0].time.percentile[90];
        call_stats_insert.insert_percentile_95 = Insert[0].time.percentile[95];
        call_stats_insert.insert_percentile_99 = Insert[0].time.percentile[99];
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
        <Table
          ref="Table"
          disableHeader={false}
          width={width}
          headerHeight={30}
          height={height}
          rowHeight={30}
          rowGetter={rowGetter}
          rowCount={list.length}
          sortBy={'index'}
        >
          <Column
            width={100}
            label="Name"
            cellRenderer={({ cellData }) => cellData}
            dataKey="name"
          />
          <Column
            width={100}
            label="Owner"
            disableSort
            dataKey="owner"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Memory"
            dataKey="memory"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Size"
            dataKey="size"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Heir"
            disableSort
            dataKey="heir"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Type"
            disableSort
            dataKey="type"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Protection"
            disableSort
            dataKey="protection"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Write concurrency"
            disableSort
            dataKey="write_concurrency"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="Read concurrency"
            disableSort
            dataKey="read_concurrency"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup max time"
            dataKey="lookup_max"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup min time"
            dataKey="lookup_min"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup median time"
            dataKey="lookup_median"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup 75th percentile time"
            dataKey="lookup_percentile_75"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup 90 percentile time"
            dataKey="lookup_percentile_90"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup 95 percentile time"
            dataKey="lookup_percentile_95"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup 99 percentile time"
            dataKey="lookup_percentile_99"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup 999 percentile time"
            dataKey="lookup_percentile_999"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="lookup count"
            dataKey="lookup_count"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert max time"
            dataKey="insert_max"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert min time"
            dataKey="insert_min"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert median time"
            dataKey="insert_median"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert 75th percentile time"
            dataKey="insert_percentile_75"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert 90 percentile time"
            dataKey="insert_percentile_90"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert 95 percentile time"
            dataKey="insert_percentile_95"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert 99 percentile time"
            dataKey="insert_percentile_99"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert 999 percentile time"
            dataKey="insert_percentile_999"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={100}
            label="insert count"
            dataKey="insert_count"
            cellRenderer={({ cellData }) => cellData}
          />
        </Table>
      )}
    </AutoSizer>
  );
};

export default TableView;
