describe('Warehouse Manager Tab', () => {
    it('should display the warehouse manager menu', () => {
        cy.visit('http://localhost:4200/WarehouseManagement/Home/WarehouseManager');

    })
    it('should display the 4 available options', () => {
        cy.get('div').contains('Create Warehouse').should('be.visible');
        cy.get('div').contains('Create Delivery').should('be.visible');
        cy.get('div').contains('Consult Warehouse').should('be.visible');
        cy.get('div').contains('Consult Deliveries').should('be.visible');
        cy.get('ul').contains('Create Warehouse').should('be.visible');
        cy.get('ul').contains('Create Delivery').should('be.visible');
        cy.get('ul').contains('Consult Warehouse').should('be.visible');
        cy.get('ul').contains('Consult Deliveries').should('be.visible');
    })    

    it('should display the introduction header', () => {
        cy.get('h1').contains('Warehouse Manager Menu').should('be.visible');
    })

    it('should display the introduction text', () => {
        cy.get('p').contains('On this page the warehouse manager can create warehouses and deliveries.').should('be.visible');
    })

    
})