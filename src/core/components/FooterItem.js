// @flow
import React from 'react';

import { OverlayTrigger, Popover } from 'react-bootstrap';

type Props = {
  title: string,
  id: string,
  item: ?React$Element<any>,
  popover: ?React$Element<any>
};

const FooterItem = ({ title, item, popover, id }: Props) => {
  return (
    <div className="Footer-item">
      <OverlayTrigger
        trigger={['hover']}
        placement="top"
        overlay={
          (
            <Popover id={id} title={title}>
              {popover}
            </Popover>
          )
        }
      >
        {item}
      </OverlayTrigger>
    </div>
  );
};

export default FooterItem;
