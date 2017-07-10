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
        >
          <Column
            width={60}
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
        </Table>
      )}
    </AutoSizer>
  );
};

export default TableView;
