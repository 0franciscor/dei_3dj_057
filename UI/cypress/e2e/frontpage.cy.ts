describe('front page', () => {
    it('passes', () => {
        cy.visit('http://localhost:4200')
    })

    it('should display the 3 available options', () => {
        cy.get('button').contains('Fleet Manager').should('be.visible');
        cy.get('button').contains('Logistics Manager').should('be.visible');
        cy.get('button').contains('Warehouse Manager').should('be.visible');
    });

    //visit the fleet manager page
    it('should visit fleet manager page', () => {
        cy.get('button').contains('Fleet Manager').click();
        cy.url().should('include', '/Logistics/Home/FleetManager');
    })

    //visit the logistics manager page
    it('should visit logistics manager page', () => {
        cy.get('button').contains('Logistics Manager').click();
        cy.url().should('include', '/Logistics/Home/LogisticsManager');
    })

    //visit the warehouse manager page
    it('should visit warehouse manager page', () => {
        cy.get('button').contains('Warehouse Manager').click();
        cy.url().should('include', '/WarehouseManagement/Home/WarehouseManager');
    })

    it('should go to front page', () => {
        cy.get('button').contains('home').click();
        cy.url().should('include', '/');
    });

    

})