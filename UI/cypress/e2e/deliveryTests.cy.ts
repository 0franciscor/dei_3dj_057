describe('Delirvery Tab', () => {

  beforeEach(() => {
    cy.visit('http://localhost:4200/Login');
    cy.wait(1000)
    cy.get('input[formcontrolname="email"]').type('franciscoredol_r03@hotmail.com');

    cy.get('input[formcontrolname="password"]').type('CappuJSON');

    cy.get('label').contains('I agree with the terms and conditions').find('span').click();

    cy.get('button').contains('Log In').click();

    cy.wait(2000);

  })

  it('should display the create delivery page and create a test', () => {

    cy.get('li').contains('Create Delivery').click();


    cy.get('mat-form-field').contains('DeliveryID');
    cy.get('mat-form-field').contains('Choose a date');
    cy.get('mat-form-field').contains('Load Time');
    cy.get('mat-form-field').contains('Unload Time');
    cy.get('mat-form-field').contains('Destination');
    cy.get('mat-form-field').contains('Delivery Mass');
    cy.get('button').contains('Create');



    cy.get('#deliveryID').type('Delivery5');

    cy.get('#deliveryDate').type('12/21/2023');

    cy.get('#loadTime').type('5');

    cy.get('#unloadTime').type('5');

    cy.get('#destination').type('5');

    cy.get('#deliveryMass').type('5');

    cy.get('#createButton').click();

    cy.get('h1').contains('Delivery: Delivery5');

  });

  it('should try to create the same delivery', () => {

    cy.get('li').contains('Create Delivery').click();

    cy.get('h2').contains('Creating a Delivery');
    cy.get('h4').contains('Please enter the information in the fields below');

    cy.get('mat-form-field').contains('DeliveryID');
    cy.get('mat-form-field').contains('Choose a date');
    cy.get('mat-form-field').contains('Load Time');
    cy.get('mat-form-field').contains('Unload Time');
    cy.get('mat-form-field').contains('Destination');
    cy.get('mat-form-field').contains('Delivery Mass');
    cy.get('button').contains('Create');


    cy.get('#deliveryID').type('Delivery5');

    cy.get('#deliveryDate').type('12/21/2023');

    cy.get('#loadTime').type('5');

    cy.get('#unloadTime').type('5');

    cy.get('#destination').type('5');

    cy.get('#deliveryMass').type('5');

    cy.get('#createButton').click();

    cy.get('h1').contains('Delivery: Delivery5');
  });

  it("should try to edit the delivery", () => {

    cy.get('li').contains('Consult Deliveries').click();

    cy.get('table').contains('Delivery5').parent().within(() => {
      cy.get('button').contains('Edit').click();
    })

    cy.get('mat-card-title').contains('Selected Delivery: Delivery5');
    cy.get('h4').contains('Please update the information in the fields below');

    cy.get('#deliveryDate').clear();
    cy.get('#deliveryDate').type('12/22/2023');


    cy.get('#loadTime').clear();
    cy.get('#loadTime').type('10');


    cy.get('#unloadTime').clear();
    cy.get('#unloadTime').type('10');


    cy.get('#destination').clear();
    cy.get('#destination').type('3');

    cy.get('#deliveryMass').clear();
    cy.get('#deliveryMass').type('10');

    cy.get('#editButton').click();

  });


  it('should display the edit delivery page and try to edit with incorrect values', () => {

    cy.get('li').contains('Consult Deliveries').click();

    cy.get('table').contains('Delivery5').parent().within(() => {
      cy.get('button').contains('Edit').click();
    })


    cy.get('mat-card-title').contains('Selected Delivery: Delivery5');
    cy.get('h4').contains('Please update the information in the fields below');



    cy.get('#deliveryDate').clear();
    cy.get('#deliveryDate').type('12/22/2023');



    cy.get('#loadTime').clear();
    cy.get('#loadTime').type('10');



    cy.get('#unloadTime').clear();
    cy.get('#unloadTime').type('10');



    cy.get('#destination').clear();
    cy.get('#destination').type('-3');


    cy.get('#deliveryMass').clear();
    cy.get('#deliveryMass').type('10');

    cy.get('#editButton').click();
  });

  it('should display the edit delivery page and delete the chosen delivery', () => {

    cy.get('li').contains('Consult Deliveries').click();

    cy.get('table').contains('Delivery5').parent().within(() => {
      cy.get('button').contains('Delete').click();
    })
  });

  it('should display the list delivery page', () => {
    cy.get('li').contains('Consult Deliveries').click();

    cy.get('th').contains('ID');
    cy.get('th').contains('Delivery Date');
    cy.get('th').contains('Load Time');
    cy.get('th').contains('Unload Time');
    cy.get('th').contains('Destination');
    cy.get('th').contains('Delivery Mass');
  });
})