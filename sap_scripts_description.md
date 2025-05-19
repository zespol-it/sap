# SAP ABAP Scripts Description / Opis skryptów SAP ABAP

## 1. FICO (Finance and Controlling) Scripts

### English:
- **create_accounting_doc**: Creates an accounting document using BAPI_ACC_DOCUMENT_POST. Handles document header, GL accounts, and currency amounts.
- **process_cost_center**: Reads cost center data from CSKS table with validity date check.

### Polski:
- **create_accounting_doc**: Tworzy dokument księgowy używając BAPI_ACC_DOCUMENT_POST. Obsługuje nagłówek dokumentu, konta księgowe i kwoty w walucie.
- **process_cost_center**: Odczytuje dane centrum kosztów z tabeli CSKS z weryfikacją daty ważności.

## 2. HR (Human Resources) Scripts

### English:
- **read_employee_data**: Reads employee master data using HR_READ_INFOTYPE function module for infotype 0001 (Organizational Assignment).
- **process_time_management**: Processes time management data for a specific employee and date using HR_READ_TIME_DATA.

### Polski:
- **read_employee_data**: Odczytuje dane podstawowe pracownika używając modułu funkcyjnego HR_READ_INFOTYPE dla infotypu 0001 (Przypisanie organizacyjne).
- **process_time_management**: Przetwarza dane zarządzania czasem dla określonego pracownika i daty używając HR_READ_TIME_DATA.

## 3. SD (Sales and Distribution) Scripts

### English:
- **create_sales_order**: Creates a sales order using BAPI_SALESORDER_CREATE with header and item data.
- **process_billing**: Creates a billing document for a sales order using BAPI_BILLINGDOC_CREATE.

### Polski:
- **create_sales_order**: Tworzy zamówienie sprzedaży używając BAPI_SALESORDER_CREATE z danymi nagłówka i pozycji.
- **process_billing**: Tworzy dokument faktury dla zamówienia sprzedaży używając BAPI_BILLINGDOC_CREATE.

## 4. MM (Materials Management) Scripts

### English:
- **create_purchase_order**: Creates a purchase order using BAPI_PO_CREATE with header and item data.
- **process_goods_receipt**: Creates a goods receipt document using BAPI_GOODSMVT_CREATE.

### Polski:
- **create_purchase_order**: Tworzy zamówienie zakupu używając BAPI_PO_CREATE z danymi nagłówka i pozycji.
- **process_goods_receipt**: Tworzy dokument przyjęcia towaru używając BAPI_GOODSMVT_CREATE.

## 5. Data Exchange Scripts

### English:
- **create_odata_service**: Creates an OData service client for remote proxy access.
- **process_idoc**: Processes IDOC data using IDOC_READ_COMPLETELY function module.
- **call_rest_service**: Makes REST service calls using HTTP client.

### Polski:
- **create_odata_service**: Tworzy klienta usługi OData dla dostępu przez proxy zdalne.
- **process_idoc**: Przetwarza dane IDOC używając modułu funkcyjnego IDOC_READ_COMPLETELY.
- **call_rest_service**: Wykonuje wywołania usług REST używając klienta HTTP.

## 6. Form Processing Scripts

### English:
- **generate_adobe_pdf**: Generates PDF documents using AdobeForms for sales orders.
- **print_sapscript_form**: Prints forms using SAPScript with header, body, and footer elements.

### Polski:
- **generate_adobe_pdf**: Generuje dokumenty PDF używając AdobeForms dla zamówień sprzedaży.
- **print_sapscript_form**: Drukuje formularze używając SAPScript z elementami nagłówka, treści i stopki.

## Usage Notes / Uwagi dotyczące użycia

### English:
1. All scripts use standard SAP function modules and BAPIs
2. Error handling is implemented for all critical operations
3. The code follows ABAP 7.4 syntax standards
4. All classes are implemented using object-oriented programming principles

### Polski:
1. Wszystkie skrypty używają standardowych modułów funkcyjnych SAP i BAPI
2. Obsługa błędów jest zaimplementowana dla wszystkich operacji krytycznych
3. Kod jest zgodny ze standardami składni ABAP 7.4
4. Wszystkie klasy są zaimplementowane zgodnie z zasadami programowania obiektowego 