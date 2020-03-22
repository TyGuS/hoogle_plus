import React from 'react';

import { Button } from 'react-bootstrap';

export const EditableRow = ({row, columns, editingRowId,
    onUpdateCell, onClickEdit, onClickSave, onClickRemove}) => {
    const rowWithOrder = columns.map(col => {return {value: row[col.name], colName:col.title}});
    const rowCells = rowWithOrder.map(({value:r}, k) => {
      return (<td key={k}>{r}</td>)
    });

    if (row.id === editingRowId) {
      const editableRowCells = rowWithOrder.map((cell, k) => {
        console.log("key", k);
        return (<td key={cell.colName}>
          <input
            key={cell.colName}
            type="text"
            className="form-control"
            onChange={onUpdateCell(cell, row)}
            value={cell.value}/>
        </td>);
      });
      return (
        <tr>
          <td>
            <Button onClick={() => onClickSave(row)}>
              Save
            </Button>
          </td>
          {editableRowCells}
        </tr>
      );
    }

    return (
      <tr>
        <td>
          <Button onClick={() => onClickEdit(row.id)}>Edit</Button>
          <Button onClick={() => onClickRemove(row.id)}>Remove</Button>
        </td>
       {rowCells}
      </tr>
    );
  };