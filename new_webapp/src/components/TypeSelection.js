import React from "react";
import { connect } from "react-redux";
import Highlight from "react-highlight.js";
import { Modal, Button, ButtonGroup, ListGroup} from "react-bootstrap";
import { setModalClosed, selectTypeFromOptions } from "../actions";

const mapStateToProps = (state) => {
    return {
        isOpen: state.modal.isOpen,
        typeOptions: state.spec.searchTypeOptions,
    };
}

const mapDispatchToProps = (dispatch) => {
    return {
        onClose: () => dispatch(setModalClosed()),
        selectType: ({typeOption}) => dispatch(selectTypeFromOptions({typeOption})),
    };
}

const TypeSelectionBase = (props) => {
    const {onClose, selectType} = props;
    const isDisabled = props.hidden || false;
    const {isOpen, typeOptions} = props;

    const mkSelection = (typeStr) => {
        return selectType({ typeOption: typeStr });
    }

    return (
        <>
        <Modal show={isOpen && !isDisabled} onHide={onClose} size="lg">
        <Modal.Header closeButton>
          <Modal.Title>Which type looks right to you?</Modal.Title>
        </Modal.Header>
        <Modal.Body>
        <div className="container">
            <div>
            To help us give you the best results,
            help us narrow down the type signature.
            Please select one of the following:
            </div>
            <br/>
            <div className="justify-content-center">
            <div>
                <ListGroup>
                    {typeOptions.map((typeStr, idx) => {
                        return (
                            <ListGroup.Item action
                                onClick={() => mkSelection(typeStr)}>
                                    <Highlight language="haskell">
                                        {typeStr}
                                    </Highlight>
                                    {/* {idx + 1} */}
                            </ListGroup.Item>);
                    })}
                </ListGroup>
            </div>
            </div>
        </div>
        </Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={onClose}>
            Close
          </Button>
        </Modal.Footer>
      </Modal>
      </>
    );
}

export const TypeSelection = connect(mapStateToProps, mapDispatchToProps)(TypeSelectionBase);