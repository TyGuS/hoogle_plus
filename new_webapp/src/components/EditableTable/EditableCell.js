import React, { cloneElement } from 'react';

import Highlight from 'react-highlight.js';

/**
 * Editable Cell:
 * Props:
 *   - cell
 *   - editingCellId : str
 *   - onUpdateCell : ({value, colName}, row) -> event -> ()
 *   - onClick : (row) -> ()
 *   - onBlur : (row) -> ()
 */
export const EditableCell = ({cell, editingColId, editingRowId, value,
    onUpdateCell, onClickEdit, onClickSave}) => {
    const onKeyPress = (event) => {
      if (event.key === "Enter") {
        onClickSave({colName: editingColId, rowName: editingRowId});
      }
    };

    const k = cell.rowId + '-' + cell.colName;

    if(cell.rowId === editingRowId) {
        const inputCell = (<input
            autoFocus={cell.rowId === editingRowId && cell.colName === editingColId}
            key={k}
            type="text"
            value={value}
            className="form-control"
            onChange={onUpdateCell({colName: cell.colName, rowName: cell.rowId})}
            onKeyPress={onKeyPress}
            />);
        if(cell.rowId === "argRow" && cell.colName !== "output") {
            return (<th key={k}>
                {inputCell}
            </th>);
        }
        if(cell.colName !== "output" || cell.rowId !== "argRow") {
            return (<td key={k}>
                {inputCell}
            </td>);
        }
    }

    if(cell.rowId === "argRow") {
        if(cell.colName !== "output"){
            return (<th
                key={k}
                onClick={() => onClickEdit({rowName: cell.rowId, colName: cell.colName})}>
                {cell.value}
            </th>);
        } else {
            return (<th key={k}>
                {cell.value}
            </th>);
        }
    }

    return (<td 
        key={k}
        onClick={() => onClickEdit({rowName: cell.rowId, colName: cell.colName})}
        >    
        <Highlight language="haskell">
          {cell.value}
        </Highlight>
    </td>);
  };