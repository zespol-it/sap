*&---------------------------------------------------------------------*
*& Report ZSAP_COMMON_SCRIPTS
*&---------------------------------------------------------------------*
REPORT zsap_common_scripts.

*----------------------------------------------------------------------*
* 1. FICO (Finance and Controlling) Examples
*----------------------------------------------------------------------*
CLASS lcl_fico_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Create Accounting Document
      create_accounting_doc
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_monat TYPE monat,
      " Process Cost Center
      process_cost_center
        IMPORTING
          iv_kostl TYPE kostl.
ENDCLASS.

CLASS lcl_fico_scripts IMPLEMENTATION.
  METHOD create_accounting_doc.
    DATA: ls_header  TYPE bapiache09,
          lt_account TYPE TABLE OF bapiacgl09,
          lt_amount  TYPE TABLE OF bapiaccr09.

    " Set document header
    ls_header-company_code = iv_bukrs.
    ls_header-fiscal_year  = iv_gjahr.
    ls_header-fiscal_period = iv_monat.
    ls_header-document_date = sy-datum.
    ls_header-posting_date = sy-datum.
    ls_header-document_type = 'SA'.

    " Create accounting document
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_header
      TABLES
        accountgl     = lt_account
        currencyamount = lt_amount
      EXCEPTIONS
        OTHERS        = 1.
  ENDMETHOD.

  METHOD process_cost_center.
    " Read cost center data
    SELECT SINGLE * FROM csks
      INTO @DATA(ls_csks)
      WHERE kostl = @iv_kostl
        AND datbi >= @sy-datum
        AND datab <= @sy-datum.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 2. HR (Human Resources) Examples
*----------------------------------------------------------------------*
CLASS lcl_hr_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Read Employee Data
      read_employee_data
        IMPORTING
          iv_pernr TYPE pernr_d,
      " Process Time Management
      process_time_management
        IMPORTING
          iv_pernr TYPE pernr_d
          iv_date  TYPE datum.
ENDCLASS.

CLASS lcl_hr_scripts IMPLEMENTATION.
  METHOD read_employee_data.
    " Read employee master data
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = iv_pernr
        infty         = '0001'
        begda         = sy-datum
        endda         = sy-datum
      TABLES
        infty_tab     = DATA(lt_p0001)
      EXCEPTIONS
        infty_not_found = 1
        OTHERS         = 2.
  ENDMETHOD.

  METHOD process_time_management.
    " Read time data
    CALL FUNCTION 'HR_READ_TIME_DATA'
      EXPORTING
        pernr         = iv_pernr
        begda         = iv_date
        endda         = iv_date
      TABLES
        time_data     = DATA(lt_time_data)
      EXCEPTIONS
        OTHERS        = 1.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 3. SD (Sales and Distribution) Examples
*----------------------------------------------------------------------*
CLASS lcl_sd_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Create Sales Order
      create_sales_order
        IMPORTING
          iv_kunnr TYPE kunnr,
      " Process Billing
      process_billing
        IMPORTING
          iv_vbeln TYPE vbak-vbeln.
ENDCLASS.

CLASS lcl_sd_scripts IMPLEMENTATION.
  METHOD create_sales_order.
    DATA: ls_header TYPE bapisdhd1,
          lt_items  TYPE TABLE OF bapisditm.

    " Set sales order header
    ls_header-doc_type = 'OR'.
    ls_header-sales_org = '1000'.
    ls_header-distr_chan = '10'.
    ls_header-division = '00'.
    ls_header-customer = iv_kunnr.

    " Create sales order
    CALL FUNCTION 'BAPI_SALESORDER_CREATE'
      EXPORTING
        sales_header_in = ls_header
      TABLES
        sales_items_in  = lt_items
      EXCEPTIONS
        OTHERS         = 1.
  ENDMETHOD.

  METHOD process_billing.
    " Create billing document
    CALL FUNCTION 'BAPI_BILLINGDOC_CREATE'
      EXPORTING
        salesdocument = iv_vbeln
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 4. MM (Materials Management) Examples
*----------------------------------------------------------------------*
CLASS lcl_mm_scripts DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Create Purchase Order
      create_purchase_order
        IMPORTING
          iv_lifnr TYPE lifnr,
      " Process Goods Receipt
      process_goods_receipt
        IMPORTING
          iv_ebeln TYPE ekko-ebeln.
ENDCLASS.

CLASS lcl_mm_scripts IMPLEMENTATION.
  METHOD create_purchase_order.
    DATA: ls_header TYPE bapimepoheader,
          lt_items  TYPE TABLE OF bapimepoitem.

    " Set purchase order header
    ls_header-doc_type = 'NB'.
    ls_header-vendor = iv_lifnr.
    ls_header-comp_code = '1000'.
    ls_header-pur_group = '001'.

    " Create purchase order
    CALL FUNCTION 'BAPI_PO_CREATE'
      EXPORTING
        po_header     = ls_header
      TABLES
        po_items      = lt_items
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.

  METHOD process_goods_receipt.
    " Create goods receipt
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = VALUE bapi2017_gm_head_01( )
      TABLES
        goodsmvt_code  = VALUE bapi2017_gm_code( )
        goodsmvt_item  = VALUE bapi2017_gm_item_create( )
      EXCEPTIONS
        OTHERS        = 1.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 5. Data Exchange Examples (OData, REST, IDOC)
*----------------------------------------------------------------------*
CLASS lcl_data_exchange DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " OData Service
      create_odata_service
        IMPORTING
          iv_entity_name TYPE string,
      " Process IDOC
      process_idoc
        IMPORTING
          iv_idoc_number TYPE edidc-docnum,
      " REST Service
      call_rest_service
        IMPORTING
          iv_url TYPE string.
ENDCLASS.

CLASS lcl_data_exchange IMPLEMENTATION.
  METHOD create_odata_service.
    " Define OData service
    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_odata_client TYPE REF TO /iwbep/if_cp_client_proxy.

    " Create OData client
    TRY.
        lo_odata_client = /iwbep/if_cp_factory_remote~create_v2_remote_proxy(
          EXPORTING
            is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
                                             proxy_model_id      = 'Z_MATERIAL_SRV'
                                             proxy_model_version = '0001' )
            io_http_client         = lo_http_client
            iv_relative_service_root = '/sap/opu/odata/sap/Z_MATERIAL_SRV' ).
      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
    ENDTRY.
  ENDMETHOD.

  METHOD process_idoc.
    " Process IDOC
    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number         = iv_idoc_number
      TABLES
        int_edidd              = DATA(lt_idoc_data)
      EXCEPTIONS
        document_not_exist     = 1
        document_number_invalid = 2
        OTHERS                 = 3.
  ENDMETHOD.

  METHOD call_rest_service.
    " Call REST service
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_response    TYPE string.

    " Create HTTP client
    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS            = 4 ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 6. Form Processing Examples (AdobeForms, SAPScript)
*----------------------------------------------------------------------*
CLASS lcl_form_processing DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Generate PDF using AdobeForms
      generate_adobe_pdf
        IMPORTING
          iv_document_number TYPE vbak-vbeln,
      " Print using SAPScript
      print_sapscript_form
        IMPORTING
          iv_form_name TYPE tdsfname.
ENDCLASS.

CLASS lcl_form_processing IMPLEMENTATION.
  METHOD generate_adobe_pdf.
    " Generate PDF using AdobeForms
    DATA: lv_fm_name TYPE funcname.
    
    lv_fm_name = 'ZFM_SALES_ORDER_PDF'.
    
    CALL FUNCTION lv_fm_name
      EXPORTING
        iv_vbeln = iv_document_number
      EXCEPTIONS
        OTHERS   = 1.
  ENDMETHOD.

  METHOD print_sapscript_form.
    " Print using SAPScript
    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        form      = iv_form_name
      EXCEPTIONS
        OTHERS    = 1.

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER'
      EXCEPTIONS
        OTHERS  = 1.

    CALL FUNCTION 'CLOSE_FORM'
      EXCEPTIONS
        OTHERS  = 1.
  ENDMETHOD.
ENDCLASS. 