import { FixedSizeVirtualScrollStrategy } from "@angular/cdk/scrolling";
import cypressConfig from "cypress.config";

describe('Create Delivery Option', () => {
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
    cy.get('#deliveryID').type('Delivery4');
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
