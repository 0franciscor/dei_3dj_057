
describe('Logistics Manager Tab', () => {

    beforeEach(() =>{
    cy.visit('http://localhost:4200/Login');
    cy.wait(1000)
    cy.get('input[formcontrolname= "email"]').type('log.manager@gmail.com');
    cy.get('input[formcontrolname= "password"]').type('Logistics123');

    cy.get('label').contains('I agree with the terms and conditions').find('span').click();

    cy.get('button').contains('Log In').click();
    
    cy.wait(4000);
    
})

    it('should display the Logistics manager menu', () => {
        cy.visit('http://localhost:4200/Logistics/Home/LogisticsManager');

    })

    it('should display the 6 available options ', ()=>{
        cy.get('div').contains('Create Path').should('be.visible');
        cy.get('div').contains('Search').should('be.visible');
        cy.get('mat-form-field').contains('Starting Warehouse').should('be.visible');
        cy.get('mat-form-field').contains('Destination Warehouse').should('be.visible');
        cy.get('div').contains('See Road Network').should('be.visible');
        cy.get('div').contains('Truck planning').should('be.visible');
        cy.get('div').contains('Create Package').should('be.visible');
        cy.get('div').contains('See Packages created').should('be.visible');

    })

    it('should display the introduction header', () => {
        cy.get('mat-card-title').contains('Logistics Manager').should('be.visible');
    })

    it('should display the introduction text', () => {
        cy.get('mat-card-subtitle').contains('Manager of Logic').should('be.visible');
    })

    it('should display the Path list', () => {
        
        cy.get('input[formControlName="startWHId"]').type('WH1');
        cy.get('button').contains('Search').click();
        cy.get('th').contains('Start Warehouse').should('be.visible');
        cy.get('th').contains('Destination warehouse').should('be.visible');
        cy.get('th').contains('Path distance').should('be.visible');
        cy.get('th').contains('Travel time').should('be.visible');
        cy.get('th').contains('Wasted energy').should('be.visible');
        cy.get('th').contains('Extra Time').should('be.visible');
    })

    it('should redirect to the path creation page', () => {
        cy.get('button').contains('Create Path').click();
        cy.url().should('include', '/Logistics/Path/CreatePath');
    })


    it('should display the path creation page', () => {
        cy.get('mat-card-title').contains('Create Path').should('be.visible');
        cy.get('form').contains('Start Warehouse ID').should('be.visible');
        cy.get('form').contains('Destination Warehouse ID').should('be.visible');
        cy.get('form').contains('Path Distance in KM').should('be.visible');
        cy.get('form').contains('Path travel time in min').should('be.visible');
        cy.get('form').contains('Wasted truck energy kw/h').should('be.visible');
        cy.get('form').contains('Extra travel time in min').should('be.visible');
    })

    it('should create the path creation page', () => {
        cy.get('input[formControlName="startWHId"]').type('t1');
        cy.get('input[formControlName="destinationWHId"]').type('t2');
        cy.get('input[formControlName="pathDistance"]').type('1000');
        cy.get('input[formControlName="pathTravelTime"]').type('1000');
        cy.get('input[formControlName="wastedEnergy"]').type('1000');
        cy.get('input[formControlName="extraTravelTime"]').type('1000');

        cy.get('button').contains('Create').click();

        cy.wait(1000);

    })

    it('should display create dialog box', () => {
        cy.get('p').contains('Error creating Path').should('be.visible');
        cy.get('button').contains('Ok').click();
    })

    it('should redirect to the logistics manager page', () => {
        cy.visit('http://localhost:4200/Logistics/Home/LogisticsManager');
    })

    it('should redirect to the truck planning page', () => {
        cy.get('button').contains('Truck planning').click();
        cy.url().should('include', '/Logistics/TruckPlanning');
    })

    it('should display the 7 available options ', ()=>{
        cy.get('mat-form-field').contains('Truck').should('be.visible');
        cy.get('mat-form-field').contains('Choose a date').should('be.visible');
        cy.get('div').contains('Find best path').should('be.visible');
        cy.get('div').contains('Highest Mass first path').should('be.visible');
        cy.get('div').contains('Closest warehouse path').should('be.visible');
        cy.get('div').contains('Cheapest Mass/Distance path').should('be.visible');
        cy.get('div').contains('Genetic Algorithm').should('be.visible');
        cy.get('div').contains('Truck Planning Simulation').should('be.visible');
        cy.get('div').contains('Get all trips').should('be.visible');
    })


    it('should redirect to the package creation page ', () => {
        cy.get('button').contains('Create Package').click();
        cy.url().should('include', '/Logistics/Package/CreatePackage')
    })

    it('should create the package creation page', () => {
        cy.get('input[formControlName= "packagingID"]').type('p1');
        cy.get('input[formControlName= "truckID"]').type('p2');
        cy.get('input[formControlName= "deliveryID "]').type('p3');
        cy.get('input[formControlName= "xPosition" ]').type('1000');
        cy.get('input[formControlName= "yPosition"]').type('1000');
        cy.get('input[formControlName = "zPosition]').type('1000');
    })

    it('should display the package list', () => {
        cy.get('button').contains('See Packages created').click();
        cy.get('th').contains('ID').should('be.visible');
        cy.get('th').contains('Truck ID').should('be.visible');
        cy.get('th').contains('Delivery ID').should('be.visible');
        cy.get('th').contains('X Position').should('be.visible');
        cy.get('th').contains('Y Position').should('be.visible');
        cy.get('th').contains('Z Position').should('be.visible');
    })


    




})