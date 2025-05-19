# SAP ABAP Scripts Documentation

## Overview / Przegląd

This repository contains a collection of commonly used SAP ABAP scripts for various business processes. The scripts are organized by module and include error handling, documentation, and best practices.

Ten repozytorium zawiera kolekcję często używanych skryptów SAP ABAP dla różnych procesów biznesowych. Skrypty są zorganizowane według modułów i zawierają obsługę błędów, dokumentację oraz najlepsze praktyki.

## Table of Contents / Spis treści

1. [FICO Scripts](#fico-scripts)
2. [HR Scripts](#hr-scripts)
3. [SD Scripts](#sd-scripts)
4. [MM Scripts](#mm-scripts)
5. [Data Exchange Scripts](#data-exchange-scripts)
6. [Form Processing Scripts](#form-processing-scripts)

## Requirements / Wymagania

- SAP NetWeaver 7.4 or higher
- ABAP Development Tools (ADT)
- Appropriate authorizations in SAP system

## Installation / Instalacja

1. Copy the ABAP code to your SAP system
2. Create a new report using transaction SE38
3. Paste the code and activate
4. Create necessary customizing entries if required

## Usage / Użycie

### FICO Scripts

#### create_accounting_doc
```abap
DATA: lo_fico TYPE REF TO lcl_fico_scripts.
CREATE OBJECT lo_fico.

DATA(lv_doc_number) = lo_fico->create_accounting_doc(
  iv_bukrs = '1000'
  iv_gjahr = '2024'
  iv_monat = '03'
).
```

#### process_cost_center
```abap
DATA(ls_cost_center) = lo_fico->process_cost_center(
  iv_kostl = '1000'
).
```

### HR Scripts

#### read_employee_data
```abap
DATA: lo_hr TYPE REF TO lcl_hr_scripts.
CREATE OBJECT lo_hr.

DATA(ls_employee) = lo_hr->read_employee_data(
  iv_pernr = '00001234'
).
```

#### process_time_management
```abap
DATA(lt_time_data) = lo_hr->process_time_management(
  iv_pernr = '00001234'
  iv_date  = sy-datum
).
```

### SD Scripts

#### create_sales_order
```abap
DATA: lo_sd TYPE REF TO lcl_sd_scripts.
CREATE OBJECT lo_sd.

DATA(lv_vbeln) = lo_sd->create_sales_order(
  iv_kunnr = '0000123456'
).
```

#### process_billing
```abap
DATA(lv_vbeln) = lo_sd->process_billing(
  iv_vbeln = '0000123456'
).
```

### MM Scripts

#### create_purchase_order
```abap
DATA: lo_mm TYPE REF TO lcl_mm_scripts.
CREATE OBJECT lo_mm.

DATA(lv_ebeln) = lo_mm->create_purchase_order(
  iv_lifnr = '0000123456'
).
```

#### process_goods_receipt
```abap
DATA(lv_mblnr) = lo_mm->process_goods_receipt(
  iv_ebeln = '0000123456'
).
```

### Data Exchange Scripts

#### create_odata_service
```abap
DATA: lo_data_exchange TYPE REF TO lcl_data_exchange.
CREATE OBJECT lo_data_exchange.

DATA(lo_client) = lo_data_exchange->create_odata_service(
  iv_entity_name = 'MATERIAL'
).
```

#### process_idoc
```abap
DATA(lt_idoc_data) = lo_data_exchange->process_idoc(
  iv_idoc_number = '0000000000123456'
).
```

#### call_rest_service
```abap
DATA(lv_response) = lo_data_exchange->call_rest_service(
  iv_url = 'https://api.example.com/data'
).
```

### Form Processing Scripts

#### generate_adobe_pdf
```abap
DATA: lo_form TYPE REF TO lcl_form_processing.
CREATE OBJECT lo_form.

DATA(lv_pdf_data) = lo_form->generate_adobe_pdf(
  iv_document_number = '0000123456'
).
```

#### print_sapscript_form
```abap
DATA(lv_success) = lo_form->print_sapscript_form(
  iv_form_name = 'ZFORM_SALES_ORDER'
).
```

## Error Handling / Obsługa błędów

All scripts include comprehensive error handling:
- BAPI return messages
- System exceptions
- Custom error messages
- Transaction handling

Wszystkie skrypty zawierają kompleksową obsługę błędów:
- Komunikaty zwrotne BAPI
- Wyjątki systemowe
- Niestandardowe komunikaty błędów
- Obsługa transakcji

## Best Practices / Najlepsze praktyki

1. **Code Organization / Organizacja kodu**
   - Use object-oriented programming
   - Implement interfaces where appropriate
   - Follow SAP naming conventions

2. **Error Handling / Obsługa błędów**
   - Always check return codes
   - Implement proper error messages
   - Use transaction handling

3. **Performance / Wydajność**
   - Use proper table access methods
   - Implement buffering where appropriate
   - Follow SAP performance guidelines

4. **Security / Bezpieczeństwo**
   - Implement proper authorization checks
   - Use secure coding practices
   - Follow SAP security guidelines

## Contributing / Współpraca

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License / Licencja

This project is licensed under the MIT License - see the LICENSE file for details.

## Support / Wsparcie

For support, please contact:
- Email: support@example.com
- SAP Service Marketplace: https://service.sap.com

## Version History / Historia wersji

- 1.0.0 (2024-03-20)
  - Initial release
  - Basic functionality for all modules
  - Error handling implementation
  - Documentation

## Acknowledgments / Podziękowania

- SAP Community
- ABAP Development Team
- All contributors 