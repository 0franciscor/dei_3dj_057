# US3111a_SPA - Como Gestor de Armazém, quero Criar, Listar e Editar um Armazém.
=======================================

# 1. Requisitos

**US3111a_SPA** Como {Gestor de Armazém} pretendo...

- US3111a_SPA.1- Criar armazéns
- US3111a_SPA.2- Listar os armazéns do sistema
- US3111a_SPA.2- Editar armazéns

### 1.1 Especificações e esclarecimentos do cliente

> [Question:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18704)
> Caro Cliente,
>Dentro das informações do Armazém (Id, Designação, Endereço, Coordenadas, Altitude) qual/quais são editáveis?
>
> [Answer:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18704)
>boa tarde,
a identificação do armazém não é editavel. a restante informação é.

> [Question:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18662)
>Caro Cliente,
Gostaríamos de saber se existem algumas restrições relativamente ao endereço, designação e identificação que a empresa especifique (como tamanho, formato, entre outros)? Se não, entendemos que as restrições são puramente as do "mundo real" - ex: moradas não podem ter numero negativos ou códigos postais devem corresponder ao formato 4 digitos - 3 digitos.
> 
> [Answer:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18662)
>bom dia,
>o endereço segue as regras de endereços postais Portugueses devendo consistir na morada, localidade e código postal, ex:
>
>Rua Dr. António Bernardino de Almeida, 431
>4249-015 Porto
>Portugal
> 
>a identificação do armazém é um código alfanumérico obrigatório com 3 caracteres.
> 
>a designação é um texto obrigatório com um máximo de 50 caracteres. normalmente será o nome da cidade/localidade onde o armazém está colocado


> [Question:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18958)
> Porque atributos gostaria que a filtragem dos armazéns fosse feita?
> 
> [Awnser:](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=18958)
>bom dia,
pelo seu identificador e designação.


# 2. Análise

- Para a desenvolvimento deste caso de uso foram adotados as práticas de Domain Drive Design juntamente com a Arquitetura Onion.
- Para respeitar as práticas de DDD foram utilizadas classes de domínios que representam ValueObjects e classes que representam Entidades do nosso agregado.
- Relativamente a arquitetura Onion, o projeto foi desenvolvido com recurso a interfaces.


## Excerto do modelo de domínio

![DM_US1002](resources/DM_US3111a.svg)

# 3. Design

- No contexto de domínio que nos foi proposto, consideramos que a entidade loja não faria sentido, visto que o papel seria o mesmo de um armazém, sendo assim, optamos por apenas representar o armazém no sistema.


## 3.1. Realização das Funcionalidades

### 3.1.1. Vista de Processos 

### Nível 1 
#### Criar um novo armazém
![SSD_US3111a_C](resources/SSD_US3111a_Create.svg)

#### Atualizar as informações de um armazém
![SSD_US3111a_U](resources/SSD_US3111a_Update.svg)

#### Listar os armazéns
![SSD_US3111a_L](resources/SSD_US3111a_List.svg)

### Nível 2

#### Criar um novo armazém
![SD_US3111a_N2_Create](resources/createWarehouseSD_N2.svg)

#### Atualizar a informação de um armazém
![SD_US3111a_N2_Update](resources/updateWarehouseSD_N2.svg)

#### Listar os armazéns
![SD_US3111a_N2_List](resources/listWarehousesSD_N2.svg)

### Nível 3

#### Criar um novo armazém
![SD_US3111a_N3_Create](resources/CreateWarehouseSD_N3.svg)

#### Atualizar a informação de um armazém
![SD_US3111a_N3_Update](resources/UpdateWarehouseSD_N3.svg)

#### Listar os armazéns
![SD_US3111a_N3_List](resources/ListWarehousesSD_N3.svg)

### 3.1.2. Vista de Cenários
![UC_US3111a](resources/UC_US3111a.svg)

### 3.1.3 Vista Lógica
![VL_US3111a](resources/Level%203%20-%20Logic%20View%20-%20WM.svg)

### 3.1.3 Vista de Implementação
![VL_US3111a](resources/Level%203%20-%20Implementation%20View.svg)

## 3.3. Testes
*Nesta secção deve sistematizar como os testes foram concebidos para permitir uma correta aferição da satisfação dos requisitos.*

### 3.3.1 Testes Unitários

**Warehouse Service**

    describe('WarehouseService', () => {
        let service: WarehouseService;
    
        beforeEach(() => {
          TestBed.configureTestingModule({});
          service = TestBed.inject(WarehouseService);
        });
    
        it('should be created', () => {
          expect(service).toBeTruthy();
        });
    
        it('should get a warehouse', async () => {
          const response = {
              "id": "TH1",
              "address": "Rua António Bernardino,47,4535-334,Porto",
              "altitude": 250,
              "latitude": "40.9321º N",
              "longitude": "8.2451º W",
              "designation": "Arouca",
              "city": "1",
            json () {
              return this;
            }
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const warehouse = await service.getWarehouse('TH1');
          expect(fetchSpy).toHaveBeenCalled();
          expect(warehouse).toEqual(response);
        });
    
    
        it('should create a warehouse', async () => {
          const response = {
            "status": 200,
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const status = await service.createWarehouse('TH1');
          expect(fetchSpy).toHaveBeenCalled();
          expect(status.status).toEqual(200);
        });
    
        it('should create a warehouse prolog', async () => {
          const response = {
            "status": 201,
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const status = await service.createWarehouseProlog('TH1');
          expect(fetchSpy).toHaveBeenCalled();
          expect(status.status).toEqual(201);
    
        });
    
        it('should update a warehouse', async () => {
          const response = {
            "status": 200,
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const status = await service.updateWarehouse('TH1');
          expect(fetchSpy).toHaveBeenCalled();
          expect(status.status).toEqual(200);
        });
    
        it('should update a warehouse prolog', async () => {
          const response = {
            "status": 200,
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const status = await service.updateWarehouseProlog('TH1');
          expect(fetchSpy).toHaveBeenCalled();
          expect(status.status).toEqual(200);
        });
    
    
        it('should get all warehouses', async () => {
          const response = {
            "warehouses": [
              {
                "id": "TH1",
                "address": "Rua António Bernardino,47,4535-334,Porto",
                "altitude": 250,
                "latitude": "40.9321º N",
                "longitude": "8.2451º W",
                "designation": "Arouca",
                "city": "1",
              }
            ],
            json () {
              return this;
            }
          };
    
          const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    
          const trucks = await service.getAllWarehouses();
          expect(fetchSpy).toHaveBeenCalled();
          expect(trucks).toEqual(response);
        });
    
        it('should send a fetch without data', async () => {
    
          const status = await service.sendFetch('test', 'GET', null);
          expect(status.status).toEqual(404);
        });
    
        it('should send a fetch with data', async () => {
          const status = await service.sendFetch('test', 'POST', "null");
          expect(status.status).toEqual(404);
        });

    });


# 4. Implementação

- Conforme o ‘design’ feito e com o agregado em questão apara o desenvolvimento desse caso de uso, os sequintes excertos de código abaixo servem para confirmar a veracidade do 'design' proposto.

### Warehouse Controller - Auth 

    async editWarehouse(req: Request, res: Response, next: NextFunction) {
        const httpAgent = new http.Agent({ rejectUnauthorized: false });

        const url = 'https://localhost:5001/api/warehouses/Update';
        
        const response = await fetch(url, {
          method: 'PUT',
          body: JSON.stringify(req.body),
          headers: {
            'Content-Type': 'application/json'
          },
          agent: httpAgent
        })
    
        if(response.status != 200){
          res.status(response.status);
          return res.json({message: "Error updating warehouse"});
        }
    }

### Warehouse Service - UI 

    async updateWarehouse(warehouse: any) {
    
        const url = this.urlOrigin+'api/warehouse/update';
        
        const data = warehouse;
        
        const response = await fetch(url, {
          method: 'PUT',
          body: JSON.stringify(data),
          headers: {
            'Content-Type': 'application/json'
          },
        })
        
        return response;
    
    }

### Warehouse Component 

    async onSubmit() {
        let answer = await this.warehouseService.createWarehouse(this.formCreateWarehouse.value);
        let message = "Warehouse created successfully";
        
        if(answer.status != 201){
          message = "Error creating warehouse";
        }
        const dialogRef = this.dialog.open(CreateWarehouseComponentDialog, {
          width: '250px',
          data: {
            name: this.formCreateWarehouse.value.Id,
            message: message},
        
        });
        
        dialogRef.afterClosed().subscribe(result => {
          if(answer.status == 200)
            this.router.navigate(['Logistics/Home/WarehouseManager']);
          
        });
    }
# 5. Demonstração da funcionalidade 

- Iremos demostra a funcionalidade de atualizar os dados de um armazém no sistema.
- Primeiro, iremos aceder a [URL](http://5.249.66.111:4200/WarehouseManagement/Home/WarehouseManager) associada a atualização de um armazém, iremos nos deparar com a seguinte página web.
![img.png](resources/img.png)
- De seguida iremos consultar todos os armazéns disponíveis no sistema e selecionaremos o que queremos atualizar.
![img_1.png](resources/img_1.png)
- Iremos atualizar o armazém de id 'TH1' e atualizaremos a sua altitude, recordo que a altitude de um armazém não pode ser negativa, caso o utilizador introduza um número negativo o armazém não será atualizado.
![img.png](resources/img_2.png)
![img_3.png](resources/img_3.png)
![img_4.png](resources/img_4.png)
![img_5.png](resources/img_5.png)
