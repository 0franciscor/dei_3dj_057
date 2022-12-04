describe('Create Warehouse Success', () => {
    it('should display the create warehouse page', () => {
      cy.visit('http://localhost:4200/WarehouseManagement/Warehouse/CreateWarehouse');
    })
  

    it('should display the warehouse form fields', () => {
      cy.get('mat-form-field').contains('WarehouseID');
      cy.get('mat-form-field').contains('Address');
      cy.get('mat-form-field').contains('Altitude');
      cy.get('mat-form-field').contains('Latitude');
      cy.get('mat-form-field').contains('Longitude');
      cy.get('mat-form-field').contains('Designation');
      cy.get('button').contains('Create');
    })
  
    it('should type in the WarehouseID', () => {
      cy.get('input[formcontrolname="Id"]').type('MH8');
    })
    it('should type in the Address', () => {
      cy.get('input[formcontrolname="Address"]').type('Rua António Bernardino,47,4535-334,Porto');
    })
    it('should type in the Altitude', () => {
      cy.get('input[formcontrolname="Altitude"]').type('356');
    })
    it('should type in the Latitude', () => {
      cy.get('input[formcontrolname="Latitude"]').type('40.9321º N');
    })
    it('should type in the Longitude', () => {
      cy.get('input[formcontrolname="Longitude"]').type('8.2451º W');
    })
    it('should type in the Designation', () => {
      cy.get('input[formcontrolname="Designation"]').type('Arouca');
    })

    it('should click the create button', () => {
        cy.wait(1000);
        cy.get('button').contains('Create').click();
    })

    it('should display create dialog box', () => {
        cy.get('p').contains('Warehouse created successfully').should('be.visible');
        cy.get('button').contains('Ok').click();
    })

  })
  
  describe('Create Warehouse Failure', () => {
    it('should display the create warehouse page', () => {
      cy.visit('http://localhost:4200/WarehouseManagement/Warehouse/CreateWarehouse');
    })
    
    it('should display the warehouse form fields', () => {
        cy.get('mat-form-field').contains('WarehouseID');
        cy.get('mat-form-field').contains('Address');
        cy.get('mat-form-field').contains('Altitude');
        cy.get('mat-form-field').contains('Latitude');
        cy.get('mat-form-field').contains('Longitude');
        cy.get('mat-form-field').contains('Designation');
        cy.get('button').contains('Create');
      })
    
      it('should type in the WarehouseID', () => {
        cy.get('input[formcontrolname="Id"]').type('MH8');
      })
      it('should type in the Address', () => {
        cy.get('input[formcontrolname="Address"]').type('Rua António Bernardino,47,4535-334,Porto');
      })
      it('should type in the Altitude', () => {
        cy.get('input[formcontrolname="Altitude"]').type('-99');
      })
      it('should type in the Latitude', () => {
        cy.get('input[formcontrolname="Latitude"]').type('40.9321º N');
      })
      it('should type in the Longitude', () => {
        cy.get('input[formcontrolname="Longitude"]').type('8.2451º W');
      })
      it('should type in the Designation', () => {
        cy.get('input[formcontrolname="Designation"]').type('Arouca');
      })
  
      it('should click the create button', () => {
          cy.wait(1000);
          cy.get('button').contains('Create').click();
      })

      it('should display create dialog box', () => {
        cy.get('p').contains('Error creating warehouse').should('be.visible');
        cy.get('button').contains('Ok').click();
    })

  })
  
  describe('Edit Warehouse option Success', () => {
    it('should display the edit warehouse page', () => {
      cy.visit('http://localhost:4200/WarehouseManagement/Warehouse/EditWarehouse/MH8');
    })
  
    it('should display the edit warehouse form', () => {
      cy.get('mat-card-title').contains('Selected Warehouse: MH8');
      cy.get('h4').contains('Please update the information in the fields below');
    });
  
    it('should modify the address', () => {
      cy.get('input[formcontrolname="address"]').clear();
      cy.get('input[formcontrolname="address"]').type('Rua Da Alegria,130,4535-334,Porto');
    })
  
    it('should modify the altitude', () => {
      cy.get('input[formcontrolname="altitude"]').clear();
      cy.get('input[formcontrolname="altitude"]').type('97');
    })
  
    it('should modify the latitude', () => {
      cy.get('input[formcontrolname="latitude"]').clear();
      cy.get('input[formcontrolname="latitude"]').type('5.9321º N');
    })
  
    it('should modify the longitude', () => {
      cy.get('input[formcontrolname="longitude"]').clear();
      cy.get('input[formcontrolname="longitude"]').type('8.2951º W');
    })
  
    it('should modify the designation', () => {
      cy.get('input[formcontrolname="designation"]').clear();
      cy.get('input[formcontrolname="designation"]').type('Porto');
    })
  
    it('should click the edit button', () => {
        cy.wait(1000);
        cy.get('button').contains('Edit').click();
    })

    it('should display create dialog box', () => {
      cy.get('p').contains('Warehouse updated successfully').should('be.visible');
      cy.get('button').contains('Ok').click();
    })
})
  
  describe('Edit Warehouse option Failure', () => {
    it('should display the edit warehouse page', () => {
        cy.visit('http://localhost:4200/WarehouseManagement/Warehouse/EditWarehouse/MH8');
      })
    
      it('should display the edit warehouse form', () => {
        cy.get('mat-card-title').contains('Selected Warehouse: MH8');
        cy.get('h4').contains('Please update the information in the fields below');
      });
    
      it('should modify the address', () => {
        cy.get('input[formcontrolname="address"]').clear();
        cy.get('input[formcontrolname="address"]').type('Rua Da Alegria,130,4535-334,Porto');
      })
    
      it('should modify the altitude', () => {
        cy.get('input[formcontrolname="altitude"]').clear();
        cy.get('input[formcontrolname="altitude"]').type('-98');
      })
    
      it('should modify the latitude', () => {
        cy.get('input[formcontrolname="latitude"]').clear();
        cy.get('input[formcontrolname="latitude"]').type('5.9321º N');
      })
    
      it('should modify the longitude', () => {
        cy.get('input[formcontrolname="longitude"]').clear();
        cy.get('input[formcontrolname="longitude"]').type('8.2951º W');
      })
    
      it('should modify the designation', () => {
        cy.get('input[formcontrolname="designation"]').clear();
        cy.get('input[formcontrolname="designation"]').type('Porto');
      })
    
      it('should click the edit button', () => {
          cy.wait(1000);
          cy.get('button').contains('Edit').click();
      })
  
      it('should display create dialog box', () => {
        cy.get('p').contains('Error updating warehouse').should('be.visible');
        cy.get('button').contains('Ok').click();
      })
  })
  
  describe('list warehouse option', () => {
     
    it('should display the list warehouse page', () => {
      cy.visit('http://localhost:4200/WarehouseManagement/Warehouse/GetWarehouseById');
    })
  
    it('should display the list warehouse form', () => {
      cy.get('th').contains('ID');
      cy.get('th').contains('Address');
      cy.get('th').contains('Altitude');
      cy.get('th').contains('Latitude');
      cy.get('th').contains('Longitude');
      cy.get('th').contains('Designation');
      cy.get('th').contains('City Id');
    })

  })