describe('Create Delivery Success', () => {
  it('should display the create delivery page', () => {
    cy.visit('http://localhost:4200/WarehouseManagement/Delivery/CreateDelivery');
  })

  it('should display the create delivery form', () => {
    cy.get('h2').contains('Creating a Delivery');
    cy.get('h4').contains('Please enter the information in the fields below');
  })

  it('should display the delivery form fields', () => {
    cy.get('mat-form-field').contains('DeliveryID');
    cy.get('mat-form-field').contains('Choose a date');
    cy.get('mat-form-field').contains('Load Time');
    cy.get('mat-form-field').contains('Unload Time');
    cy.get('mat-form-field').contains('Destination');
    cy.get('mat-form-field').contains('Delivery Mass');
    cy.get('button').contains('Create');
  })

  it('should type in the deliveryID', () => {
    cy.get('#deliveryID').type('Delivery5');
  })
  it('should type in the date', () => {
    cy.get('#deliveryDate').type('12/21/2023');
  })
  it('should type in the load time', () => {
    cy.get('#loadTime').type('5');
  })
  it('should type in the unload time', () => {
    cy.get('#unloadTime').type('5');
  })
  it('should type in the destination', () => {
    cy.get('#destination').type('5');
  })
  it('should type in the delivery mass', () => {
    cy.get('#deliveryMass').type('5');
  })

  it('should click the create button', () => {
    cy.get('#createButton').click();
  })

  it('should display the delivery created message', () => {
    cy.get('p').contains('Delivery created successfully');
  })
})

describe('Create Delivery Failure', () => {
  it('should display the create delivery page', () => {
    cy.visit('http://localhost:4200/WarehouseManagement/Delivery/CreateDelivery');
  })

  it('should display the create delivery form', () => {
    cy.get('h2').contains('Creating a Delivery');
    cy.get('h4').contains('Please enter the information in the fields below');
  })

  it('should display the delivery form fields', () => {
    cy.get('mat-form-field').contains('DeliveryID');
    cy.get('mat-form-field').contains('Choose a date');
    cy.get('mat-form-field').contains('Load Time');
    cy.get('mat-form-field').contains('Unload Time');
    cy.get('mat-form-field').contains('Destination');
    cy.get('mat-form-field').contains('Delivery Mass');
    cy.get('button').contains('Create');
  })

  it('should type in the deliveryID', () => {
    cy.get('#deliveryID').type('Delivery5');
  })
  it('should type in the date', () => {
    cy.get('#deliveryDate').type('12/21/2023');
  })
  it('should type in the load time', () => {
    cy.get('#loadTime').type('5');
  })
  it('should type in the unload time', () => {
    cy.get('#unloadTime').type('5');
  })
  it('should type in the destination', () => {
    cy.get('#destination').type('5');
  })
  it('should type in the delivery mass', () => {
    cy.get('#deliveryMass').type('5');
  })

  it('should click the create button', () => {
    cy.get('#createButton').click();
  })

  it('should display the delivery failed message', () => {
    cy.get('p').contains('Delivery creation failed');
  })
})

describe('Edit Delivery option Success', () => {
  it('should display the edit delivery page', () => {
    cy.visit('http://localhost:4200/WarehouseManagement/Delivery/EditDelivery/Delivery5');
  })

  it('should display the edit delivery form', () => {
    cy.get('mat-card-title').contains('Selected Delivery: Delivery5');
    cy.get('h4').contains('Please update the information in the fields below');
  });

  it('should modify the date', () => {
    cy.get('#deliveryDate').clear();
    cy.get('#deliveryDate').type('12/22/2023');
  })

  it('should modify the load time', () => {
    cy.get('#loadTime').clear();
    cy.get('#loadTime').type('10');
  })

  it('should modify the unload time', () => {
    cy.get('#unloadTime').clear();
    cy.get('#unloadTime').type('10');
  })

  it('should modify the destination', () => {
    cy.get('#destination').clear();
    cy.get('#destination').type('3');
  })

  it('should modify the delivery mass', () => {
    cy.get('#deliveryMass').clear();
    cy.get('#deliveryMass').type('10');
  })

  it('should click the edit button', () => {
    cy.get('#editButton').click();
  })

  it('should display the delivery edited message', () => {
    cy.get('p').contains('Delivery Edited successfully');
  })
})

describe('Edit Delivery option Failure', () => {
  it('should display the edit delivery page', () => {
    cy.visit('http://localhost:4200/WarehouseManagement/Delivery/EditDelivery/Delivery5');
  })

  it('should display the edit delivery form', () => {
    cy.get('mat-card-title').contains('Selected Delivery: Delivery5');
    cy.get('h4').contains('Please update the information in the fields below');
  });

  it('should modify the date', () => {
    cy.get('#deliveryDate').clear();
    cy.get('#deliveryDate').type('12/22/2023');
  })

  it('should modify the load time', () => {
    cy.get('#loadTime').clear();
    cy.get('#loadTime').type('10');
  })

  it('should modify the unload time', () => {
    cy.get('#unloadTime').clear();
    cy.get('#unloadTime').type('10');
  })

  it('should modify the destination', () => {
    cy.get('#destination').clear();
    cy.get('#destination').type('-3');
  })

  it('should modify the delivery mass', () => {
    cy.get('#deliveryMass').clear();
    cy.get('#deliveryMass').type('10');
  })

  it('should click the edit button', () => {
    cy.get('#editButton').click();
  })

  it('should display the delivery edited message', () => {
    cy.get('p').contains('Error updating Delivery');
  })
})

describe('list delivery option', () => {
  it('should display the list delivery page', () => {
    cy.visit('http://localhost:4200/WarehouseManagement/Delivery/GetDelivery');
  })

  it('should display the list delivery form', () => {
    cy.get('th').contains('ID');
    cy.get('th').contains('Delivery Date');
    cy.get('th').contains('Load Time');
    cy.get('th').contains('Unload Time');
    cy.get('th').contains('Destination');
    cy.get('th').contains('Delivery Mass');
  })
})