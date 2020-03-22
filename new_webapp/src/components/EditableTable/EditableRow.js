import React from 'react';

import { Button } from 'react-bootstrap';
import Highlight from 'react-highlight.js';

export const EditableRow = ({row, columns, editingRowId,
    onUpdateCell, onClickEdit, onClickSave, onClickRemove}) => {
    const rowWithOrder = columns.map(col => {return {value: row[col.name], colName:col.title}});
    const rowCells = rowWithOrder.map(({value:r}, k) => {
      return (
        <td key={k}>
          <Highlight language="haskell">
            {r}
          </Highlight>
        </td>)
    });

    if (row.id === editingRowId) {
      const editableRowCells = rowWithOrder.map((cell, k) => {
        return (<td key={cell.colName}>
          <input
            autoFocus={k === 0}
            key={cell.colName}
            type="text"
            className="form-control"
            onChange={onUpdateCell(cell, row)}
            value={cell.value}/>
        </td>);
      });
      return (
        <tr>
          {editableRowCells}
          <td className="row_controls">
            <Button
              variant="link"
              onClick={() => onClickSave(row)}
            >
              Save
            </Button>
          </td>
        </tr>
      );
    }

    return (
      <tr>
       {rowCells}
        <td className="row_controls">
          <Button
            variant="link"
            onClick={() => onClickEdit(row.id)}
          >
            Edit
          </Button>
          <Button
            variant="link"
            onClick={() => onClickRemove(row.id)}
          >
            Remove
          </Button>
        </td>
      </tr>
    );
  };