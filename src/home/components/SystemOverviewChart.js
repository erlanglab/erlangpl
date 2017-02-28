// @flow
import React from 'react';
import { Col } from 'react-bootstrap';
import {
  AreaChart,
  Area,
  YAxis,
  XAxis,
  CartesianGrid,
  Tooltip
} from 'recharts';

import core from '../../core';
const { Loader } = core.components;

type Props = {
  data: any,
  color: string,
  title: string,
  width: number,
  height: number,
  dataKey: string,
  loaderText: string,
  domain?: any,
  xAxisDataKey?: string
};

const SystemOverviewChart = (
  {
    data,
    color,
    title,
    width,
    height,
    dataKey,
    xAxisDataKey,
    loaderText,
    domain
  }: Props
) => {
  return (
    <Col xs={12}>
      <h5 className="SystemInfo-list-header">
        {title}
      </h5>
      {data.length > 1
        ? <AreaChart
            width={width}
            height={height}
            data={data}
            margin={{ top: 10, right: 0, left: 0, bottom: 0 }}
          >
            <XAxis hide={true} dataKey={xAxisDataKey || 'name'} />
            <YAxis domain={domain || [0, 'auto']} />
            <CartesianGrid strokeDasharray="3 3" />
            <Tooltip />
            <Area
              isAnimationActive={false}
              type="linear"
              dataKey={dataKey}
              stroke={color}
              fillOpacity={1}
              fill={color}
            />
          </AreaChart>
        : <Loader style={{ height }} text={loaderText} />}
    </Col>
  );
};

export default SystemOverviewChart;
