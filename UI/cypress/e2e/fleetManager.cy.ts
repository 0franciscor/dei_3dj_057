describe('Fleet Manager Tab', () => {

    beforeEach(() => {
        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('flt.manager@gmail.com');

        cy.get('input[formcontrolname="password"]').type('Fleet123');

        cy.get('label').contains('I agree with the terms and conditions').find('span').click();
        

        cy.get('button').contains('Log In').click();
        
        cy.wait(2000);

    })

    after(() => {
        //get mat-icon named logout and click
        cy.get('mat-icon').contains('logout').click();
        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('firewall.norton@gmail.com');

        cy.get('input[formcontrolname="password"]').type('CappuJSON');

        cy.get('label').contains('I agree with the terms and conditions').find('span').click();
        

        cy.get('button').contains('Log In').click();
        cy.wait(1000);
        cy.get('button').contains('Fleet Manager').click();
        cy.wait(1000);

        cy.get('button').contains('See All').click();
        //get row containing ID "cypressTestTruck" and press delete button
        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('td').contains('9001').should('be.visible');
            cy.get('button').contains('Disable Truck').click();
        })
        
        cy.get('p').contains('Truck Disabled Successfully').should('be.visible');
        
        cy.get('button').contains('Ok').click();

        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('td').contains('9001').should('be.visible');
            cy.get('button').contains('Delete Truck').click();
        })

        cy.get('p').contains('Truck Deleted Successfully').should('be.visible');

        cy.get('button').contains('Ok').click();
        

    })


    it('should display page correctly', () => {

        cy.get('div').contains('Create Truck').should('be.visible');
        cy.get('div').contains('See All').should('be.visible');
        cy.get('mat-form-field').contains('Truck').should('be.visible');
        cy.get('button').contains('Create Truck').should('be.visible');
        cy.get('button').contains('See All').should('be.visible');
        cy.get('mat-label').contains('Truck').should('be.visible');
        cy.get('mat-card-title').contains('Fleet Manager').should('be.visible');
        cy.get('mat-card-subtitle').contains('ITS TRUCK MONTH!!').should('be.visible');
    })    

    
    it('should display the truck list', () => {
        cy.get('button').contains('See All').click();
        cy.get('th').contains('TruckID').should('be.visible');
        cy.get('th').contains('Tare').should('be.visible');
        cy.get('th').contains('Capacity').should('be.visible');
        cy.get('th').contains('Maximum Battery Capacity').should('be.visible');
        cy.get('th').contains('Autonomy').should('be.visible');
        cy.get('th').contains('Fast Charge Time').should('be.visible');
        cy.get('th').contains('Actions').should('be.visible');
    })


    it('should create a truck', () => {
        cy.get('button').contains('Create Truck').click();
        cy.url().should('include', '/Logistics/Truck/CreateTruck');
        cy.get('mat-card-title').contains('Create Truck').should('be.visible');
        cy.get('form').contains('TruckID').should('be.visible');
        cy.get('form').contains('Tare').should('be.visible');
        cy.get('form').contains('Capacity').should('be.visible');
        cy.get('form').contains('Maximum Battery Capacity').should('be.visible');
        cy.get('form').contains('Autonomy').should('be.visible');
        cy.get('form').contains('Fast Charge Time').should('be.visible');
        cy.get('input[formcontrolname="truckID"]').type('cypressTestTruck');

        cy.get('input[formcontrolname="tare"]').type('1000');

        cy.get('input[formcontrolname="capacity"]').type('1000');
 
        cy.get('input[formcontrolname="maxBatteryCapacity"]').type('1000');

        cy.get('input[formcontrolname="autonomy"]').type('1000');

        cy.get('input[formcontrolname="fastChargeTime"]').type('1000');

        cy.get('button').contains('Create').click();

        cy.get('p').contains('Truck created successfully').should('be.visible');
        cy.get('button').contains('Ok').click();


    })


    it('should edit a created truck', () => {
        cy.get('button').contains('See All').click();
        //get row containing ID "cypressTestTruck" and press edit button
        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('button').contains('Edit').click();
        })
        //edit truck
        cy.get('input[formcontrolname="tare"]').clear().type('9001');

        cy.get('input[formcontrolname="capacity"]').clear().type('9001');
 
        cy.get('input[formcontrolname="maxBatteryCapacity"]').clear().type('9001');

        cy.get('input[formcontrolname="autonomy"]').clear().type('9001');

        cy.get('input[formcontrolname="fastChargeTime"]').clear().type('9001');

        cy.get('button').contains('Edit').click();

        cy.get('p').contains('Truck updated successfully').should('be.visible');
        cy.get('button').contains('Ok').click();


    })


    it('should inhibit a created truck and activate it after', () => {

        cy.get('button').contains('See All').click();
        //get row containing ID "cypressTestTruck" and press delete button
        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('td').contains('9001').should('be.visible');
            cy.get('button').contains('Disable Truck').click();
        })
        
        cy.get('p').contains('Truck Disabled Successfully').should('be.visible');
        
        cy.get('button').contains('Ok').click();

        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('td').contains('9001').should('be.visible');
            cy.get('button').contains('Enable Truck').click();
        })
        cy.get('p').contains('Truck Enabled Successfully').should('be.visible');
        
        cy.get('button').contains('Ok').click();


    });



})