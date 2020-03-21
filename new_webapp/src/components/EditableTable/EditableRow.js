import React from 'react';

import { Button, ButtonGroup, Table, InputGroup, Form } from 'react-bootstrap';

export const EditableRow = ({row, columns, editingRowId,
    onUpdateCell, onClickEdit}) => {
    const rowWithOrder = columns.map(col => {return {value: row[col.name], colName:col.title}});
    const rowCells = rowWithOrder.map(({value:r}, k) => {
      console.log("rowWithOrder", r);
      return (<td key={k}>{r}</td>)
    });

    if (row.id === editingRowId) {
      console.log("drawing editing row", row);
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
          <td></td>
          {editableRowCells}
        </tr>
      );
    }

    return (
      <tr>
        <td>
          <Button onClick={onClickEdit(row.id)}>edit</Button>
          <Button>remove</Button>
        </td>
       {rowCells}
      </tr>
    );
  };