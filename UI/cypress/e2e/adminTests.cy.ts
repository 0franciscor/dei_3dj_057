describe('Admin Tab', () => {

    beforeEach(() => {

        cy.visit('http://localhost:4200/Login');
        cy.wait(1000)
        cy.get('input[formcontrolname="email"]').type('miguel.alecrim@hotmail.com');
        cy.get('input[formcontrolname="password"]').type('CappuJSON');
        cy.get('label').contains('I agree with the terms and conditions').find('span').click();

        cy.get('button').contains('Log In').click();

        cy.wait(2000);
    })

    it('should display page correctly', () => {

        cy.get('div').contains('Create User').should('be.visible');
        cy.get('div').contains('Cancel User').should('be.visible');
        cy.get('mat-card-title').contains('Admin').should('be.visible');
        cy.get('mat-card-subtitle').contains('yes').should('be.visible');
    })

    it('should display the Cancel user page correctly', () => {
        cy.get('button').contains('Cancel User').click();
        cy.url().should('include', 'Admin/CancelUser');
        cy.get('mat-card-title').contains('List of Users').should('be.visible');
        cy.get('mat-card-subtitle').contains('A place where you can check a users info').should('be.visible');
        cy.get('mat-label').contains('User').should('be.visible');
        cy.get('button').contains('Go Back').click();
        cy.url().should('include', 'Admin/Home');
    })




    it('should create a User', () => {
        cy.get('button').contains('Create User').click();
        cy.url().should('include', 'Admin/CreateUser');
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
        cy.get('mat-label').contains('Role').should('be.visible');
        cy.get('button').contains('Create').click();

    })







})