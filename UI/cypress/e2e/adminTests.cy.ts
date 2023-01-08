describe('Admin Tab', () => {

    it('should display page correctly', () => {

        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('miguel.alecrim@hotmail.com');
        cy.get('input[formcontrolname="password"]').type('CappuJSON');
        cy.get('label').contains('I agree with the terms and conditions').find('span').click();

        cy.get('button').contains('Log In').click();

        cy.wait(2000);


        cy.get('div').contains('Create User').should('be.visible');
        cy.get('div').contains('Cancel User').should('be.visible');
        cy.get('mat-card-title').contains('Admin').should('be.visible');
        cy.get('mat-card-subtitle').contains('yes').should('be.visible');
    }) 

    it('should create a User', () => {

        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('miguel.alecrim@hotmail.com');
        cy.get('input[formcontrolname="password"]').type('CappuJSON');
        cy.get('label').contains('I agree with the terms and conditions').find('span').click();

        cy.get('button').contains('Log In').click();

        cy.wait(2000);


        cy.get('button').contains('Create User').click();

        cy.get('mat-card-title').contains('Create User').should('be.visible');
        cy.get('form').contains('First Name').should('be.visible');
        cy.get('form').contains('Last Name').should('be.visible');
        cy.get('form').contains('Email').should('be.visible');
        cy.get('form').contains('Password').should('be.visible');
        cy.get('form').contains('Phone Number').should('be.visible');
        cy.get('form').contains('Role').should('be.visible');

        cy.get('input[formcontrolname ="firstName"]').type('João');
        cy.get('input[formcontrolname="lastName"]').type('Santos');
        cy.get('input[formcontrolname="email"]').type('joao_santos@gmail.com');
        cy.get('input[formcontrolname="password"]').type('tomorrowandtomorrow');
        cy.get('input[formcontrolname="phoneNumber"]').type('857462816');
        
        cy.get('[name=role]').select('Logistics Manager');
        
        cy.get('button').contains('Create').click();

    })

    it('should cancel a User', () => {

        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('joao_santos@gmail.com');
        cy.get('input[formcontrolname="password"]').type('tomorrowandtomorrow');
        cy.get('label').contains('I agree with the terms and conditions').find('span').click();

        cy.get('button').contains('Log In').click();

        cy.wait(2000);

        cy.get('button').contains('account_circle').click();
        
        cy.get('#deletar').click();

    });
})