'use babel';

export default class TestAtomPackageView {

  constructor(serializedState) {
    // Create root element
    this.element = document.createElement('div');
    this.element.classList.add('test-atom-package');

    // Create message element
    const message = document.createElement('div');
    message.textContent = 'The TestAtomPackage package is Alive! It\'s ALIVE!';
    message.classList.add('message');
    this.element.appendChild(message);
  }

  // Returns an object that can be retrieved when package is activated
  serialize() {}
  setCount(count) {
  //const displayText = `There are ${count} words.`;
    this.element.children[0].textContent = count;
    }

  // Tear down any state and detach
  destroy() {
    this.element.remove();
  }

  getElement() {
    return this.element;
  }

}

//# ll
//# ll
//# l2
//public static FieldsConfig getFieldsConfig() {}
