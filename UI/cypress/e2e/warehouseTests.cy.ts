describe('Warehouse Management tab', () => {

    beforeEach(() => {
      cy.visit('http://localhost:4200/Login');
      cy.wait(1000)
      cy.get('input[formcontrolname="email"]').type('wh.Manager@gmail.com');

      cy.get('input[formcontrolname="password"]').type('Warehouse123');

      cy.get('label').contains('I agree with the terms and conditions').find('span').click();
      

      cy.get('button').contains('Log In').click();
      
      cy.wait(2000);

    })

  
    it('Create Warehouse Success', () => {
      cy.get('li').contains('Create Warehouse').click();
      cy.url().should('include', '/WarehouseManagement/Warehouse/CreateWarehouse');
      cy.get('mat-form-field').contains('WarehouseID');
      cy.get('mat-form-field').contains('Address');
      cy.get('mat-form-field').contains('Altitude');
      cy.get('mat-form-field').contains('Latitude');
      cy.get('mat-form-field').contains('Longitude');
      cy.get('mat-form-field').contains('Designation');
      cy.get('button').contains('Create');

      cy.get('input[formcontrolname="Id"]').type('MH8');
      cy.get('input[formcontrolname="Address"]').type('Rua António Bernardino,47,4535-334,Porto');
      cy.get('input[formcontrolname="Altitude"]').type('356');
      cy.get('input[formcontrolname="Latitude"]').type('40.9321º N');
      cy.get('input[formcontrolname="Longitude"]').type('8.2451º W');
      cy.get('input[formcontrolname="Designation"]').type('Arouca');

      cy.wait(1000);
      cy.get('button').contains('Create').click();

      cy.get('p').contains('Warehouse created successfully').should('be.visible');
      cy.get('button').contains('Ok').click();
    })

    it('Create Warehouse Failure', () => {
      cy.get('li').contains('Create Warehouse').click();
      cy.url().should('include', '/WarehouseManagement/Warehouse/CreateWarehouse');
      cy.get('mat-form-field').contains('WarehouseID');
      cy.get('mat-form-field').contains('Address');
      cy.get('mat-form-field').contains('Altitude');
      cy.get('mat-form-field').contains('Latitude');
      cy.get('mat-form-field').contains('Longitude');
      cy.get('mat-form-field').contains('Designation');
      cy.get('button').contains('Create');

      cy.get('input[formcontrolname="Id"]').type('MH8');
      cy.get('input[formcontrolname="Address"]').type('Rua António Bernardino,47,4535-334,Porto');
      cy.get('input[formcontrolname="Altitude"]').type('-99');
      cy.get('input[formcontrolname="Latitude"]').type('40.9321º N');
      cy.get('input[formcontrolname="Longitude"]').type('8.2451º W');
      cy.get('input[formcontrolname="Designation"]').type('Arouca');

      cy.wait(1000);
      cy.get('button').contains('Create').click();

      cy.get('p').contains('Error creating warehouse').should('be.visible');
      cy.get('button').contains('Ok').click();
    })
    
    it('Edit Warehouse option Success', () => {
      cy.get('li').contains('Consult Warehouse').click();
      cy.get('#warehouseId').then($warehouseId => {
        if($warehouseId.text() === 'MH8'){

          cy.get('button').contains('Edit').click();
          cy.url().should('include', 'Warehouse/EditWarehouse/MH8');

          cy.get('mat-card-title').contains('Selected Warehouse: MH8');
          cy.get('h4').contains('Please update the information in the fields below');

          cy.get('input[formcontrolname="address"]').clear();
          cy.get('input[formcontrolname="address"]').type('Rua Da Alegria,130,4535-334,Porto');

          cy.get('input[formcontrolname="altitude"]').clear();
          cy.get('input[formcontrolname="altitude"]').type('97');

          cy.get('input[formcontrolname="latitude"]').clear();
          cy.get('input[formcontrolname="latitude"]').type('5.9321º N');

          cy.get('input[formcontrolname="longitude"]').clear();
          cy.get('input[formcontrolname="longitude"]').type('8.2951º W');

          cy.get('input[formcontrolname="designation"]').clear();
          cy.get('input[formcontrolname="designation"]').type('Porto');

          cy.wait(1000);
          cy.get('button').contains('Edit').click();

          cy.get('p').contains('Warehouse updated successfully').should('be.visible');
          cy.get('button').contains('Ok').click();
        }

      })
      
    })

    it('Edit Warehouse option Failure', () => {
      cy.get('li').contains('Consult Warehouse').click();
      cy.get('#warehouseId').then($warehouseId => {
        if($warehouseId.text() === 'MH8'){
          cy.get('button').contains('Edit').click();

          cy.url().should('include', 'Warehouse/EditWarehouse/MH8');

          cy.get('mat-card-title').contains('Selected Warehouse: MH8');
          cy.get('h4').contains('Please update the information in the fields below');
    
          cy.get('input[formcontrolname="address"]').clear();
          cy.get('input[formcontrolname="address"]').type('Rua Da Alegria,130,4535-334,Porto');
    
          cy.get('input[formcontrolname="altitude"]').clear();
          cy.get('input[formcontrolname="altitude"]').type('-98');
    
          cy.get('input[formcontrolname="latitude"]').clear();
          cy.get('input[formcontrolname="latitude"]').type('5.9321º N');
    
          cy.get('input[formcontrolname="longitude"]').clear();
          cy.get('input[formcontrolname="longitude"]').type('8.2951º W');
    
          cy.get('input[formcontrolname="designation"]').clear();
          cy.get('input[formcontrolname="designation"]').type('Porto');
    
          cy.wait(1000);
          cy.get('button').contains('Edit').click();
    
          cy.get('p').contains('Warehouse updated successfully').should('be.visible');
          cy.get('button').contains('Ok').click();
        }

      })
     
    })

    it('Should verify if the Activate and Deactivate Button works', () => {
      
      cy.get('li').contains('Consult Warehouse').click();
      cy.url().should('include', 'Warehouse/GetWarehouseById');

      cy.get('th').contains('ID');
      cy.get('th').contains('Address');
      cy.get('th').contains('Altitude');
      cy.get('th').contains('Latitude');
      cy.get('th').contains('Longitude');
      cy.get('th').contains('Designation');
      cy.get('th').contains('City Id');
      cy.get('th').contains('Active');
      cy.get('th').contains('Actions');

      cy.get('#active').then($active => {
        if($active){
          cy.get('button').contains('Edit');
        }
      })

      cy.get('#active').then($active => {
        if(!$active){
          cy.get('button').contains('Activate').click();
          cy.visit('http://localhost:4200/WarehouseManagement/Home/WarehouseManager');
        }
      })

      cy.get('#active').then($active => {
        if($active){
          cy.get('button').contains('Deactivate').click();
          cy.visit('http://localhost:4200/WarehouseManagement/Home/WarehouseManager');
        }

      })

      

    })

  })
  
  

