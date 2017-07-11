// @flow
import React from 'react';

import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
import { Table, Column } from 'react-virtualized/dist/commonjs/Table';
import 'react-virtualized/styles.css';

const TableView = ({ table }) => {
  if (table.length < 1) return null;
  const list = table[0].tabs.map(({ info, ...a }) => ({ ...a, ...info }));
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
          sort={'size'}
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
            sortBy="memory"
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
            width={150}
            label="Write concurrency"
            disableSort
            dataKey="write_concurrency"
            cellRenderer={({ cellData }) => cellData}
          />
          <Column
            width={150}
            label="Read concurrency"
            disableSort
            dataKey="read_concurrency"
            cellRenderer={({ cellData }) => cellData}
          />
        </Table>
      )}
    </AutoSizer>
  );
};

export default TableView;
