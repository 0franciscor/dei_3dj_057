using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System.Runtime.Intrinsics.X86;

namespace WarehouseManagementTest.Controllers.Deliveries
{
    [TestFixture]
    internal class DeliveryControllerTest
    {
        private string? id;

        private DateTime deliveryDate;

        private float loadTime;

        private float unloadTime;

        private string? destination;

        private float deliveryMass;

        [SetUp]
        public void Setup()
        {
            id = "testID";
            deliveryDate = new DateTime(2020, 12, 12);
            loadTime = 10;
            unloadTime = 20;
            destination = "testDestination";
            deliveryMass = 30;

        }

        //SETUP LISTS FOR GETALL()
        private List<DeliveryDTO> GetDeliveriesList()
        {
            var deliveryList = new List<DeliveryDTO>
            {
                new DeliveryDTO(){deliveryID = id, deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID2", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID3", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID4", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID5", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
            };

            return deliveryList;
        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            Assert.That(service, Is.Not.Null);
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            
            var serviceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            serviceMock.Setup(repo => repo.GetDeliveries()).ReturnsAsync(expectedList);


            var deliveryController = new DeliveryController(serviceMock.Object);
            var resultList = await deliveryController.GetAll();

            for(int i = 0; i < expectedList.Count; i++)
            {
                Assert.That(resultList.Value[i].deliveryID, Is.EqualTo(expectedList[i].deliveryID)); 
            }
        }

        [Test]
        public async Task GetByIDTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();
            var deliveryID = new DeliveryID(id);

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);

            deliveryServiceMock.Setup(repo => repo.GetDelivery(deliveryID)).ReturnsAsync(deliveryExpected);

            var deliveryController = new DeliveryController(deliveryServiceMock.Object);

            var aux = await deliveryController.GetByID(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as OkObjectResult).Value);
                
                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }

        [Test]
        public async Task CreateTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();

            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();
            
            var deliveryServiceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            
            deliveryServiceMock.Setup(repo => repo.CreateDelivery(deliveryExpected)).ReturnsAsync(deliveryExpected);
            var deliveryController = new DeliveryController(deliveryServiceMock.Object);

            var aux = await deliveryController.Post(deliveryExpected);

            if (aux == null)
                Assert.Fail();
            else {
                var deliveryResult = ((DeliveryDTO)(aux.Result as CreatedAtActionResult).Value);

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }
        
        
        

    }
}
