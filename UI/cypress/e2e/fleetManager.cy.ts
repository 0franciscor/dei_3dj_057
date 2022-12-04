describe('Fleet Manager Tab', () => {
    it('should display the fleet manager menu', () => {
        cy.visit('http://localhost:4200/Logistics/Home/FleetManager');

    })
    it('should display the 3 available options', () => {
        cy.get('div').contains('Create Truck').should('be.visible');
        cy.get('div').contains('See All').should('be.visible');
        cy.get('mat-form-field').contains('Truck').should('be.visible');
        cy.get('button').contains('Create Truck').should('be.visible');
        cy.get('button').contains('See All').should('be.visible');
        cy.get('mat-label').contains('Truck').should('be.visible');
    })    
    it('should display the introduction header', () => {
        cy.get('mat-card-title').contains('Fleet Manager').should('be.visible');
    })

    it('should display the introduction text', () => {
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


    it('should redirect to the truck creation page', () => {
        cy.get('button').contains('Create Truck').click();
        cy.url().should('include', '/Logistics/Truck/CreateTruck');
    })

    it('should display the truck creation page', () => {
        cy.get('mat-card-title').contains('Create Truck').should('be.visible');
        cy.get('form').contains('TruckID').should('be.visible');
        cy.get('form').contains('Tare').should('be.visible');
        cy.get('form').contains('Capacity').should('be.visible');
        cy.get('form').contains('Maximum Battery Capacity').should('be.visible');
        cy.get('form').contains('Autonomy').should('be.visible');
        cy.get('form').contains('Fast Charge Time').should('be.visible');
    })

    it('should create a truck', () => {

        cy.get('input[formcontrolname="truckID"]').type('cypressTestTruck');

        cy.get('input[formcontrolname="tare"]').type('1000');

        cy.get('input[formcontrolname="capacity"]').type('1000');
 
        cy.get('input[formcontrolname="maxBatteryCapacity"]').type('1000');

        cy.get('input[formcontrolname="autonomy"]').type('1000');

        cy.get('input[formcontrolname="fastChargeTime"]').type('1000');

        cy.get('button').contains('Create').click();

        cy.wait(1000);

    });

    it('should display create dialog box', () => {
        cy.get('p').contains('Truck created successfully').should('be.visible');
        cy.get('button').contains('Ok').click();
    })

    it('should redirect to the fleet manager page', () => {
        cy.visit('http://localhost:4200/Logistics/Home/FleetManager');
    })

    it('should edit created truck', () => {
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

        cy.wait(1000);

    })

    it('should display edit dialog box', () => {
        
        cy.get('p').contains('Truck updated successfully').should('be.visible');
        cy.get('button').contains('Ok').click();
    })

    it('should redirect to the fleet manager page', () => {
        cy.visit('http://localhost:4200/Logistics/Home/FleetManager');
    })

    it('should delete created truck', () => {

        cy.get('button').contains('See All').click();
        //get row containing ID "cypressTestTruck" and press delete button
        cy.get('tr').contains('cypressTestTruck').parent().within(() => {
            cy.get('td').contains('9001').should('be.visible');
            cy.get('button').contains('Delete').click();
        })
        
        cy.wait(1000);

    });

    it('should display delete dialog box', () => {
        cy.get('p').should('be.visible');
        
        cy.get('button').contains('Ok').click();
    })

    it('should redirect to the fleet manager page', () => {
        cy.visit('http://localhost:4200/Logistics/Home/FleetManager');
    })


})