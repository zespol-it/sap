*&---------------------------------------------------------------------*
*& Report ZABAP_EXAMPLES
*&---------------------------------------------------------------------*
REPORT zabap_examples.

*----------------------------------------------------------------------*
* 1. Basic ABAP 7.4 Syntax Examples
*----------------------------------------------------------------------*
CLASS lcl_examples DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      show_basic_syntax,
      show_oo_example,
      show_data_exchange,
      show_form_processing.
ENDCLASS.

CLASS lcl_examples IMPLEMENTATION.
  METHOD show_basic_syntax.
    " Inline declarations
    DATA(lt_materials) = VALUE mara_t( ).
    
    " Constructor expressions
    DATA(lo_material) = NEW zcl_material( ).
    
    " String templates
    DATA(lv_message) = |Material { sy-mandt } created successfully|.
    
    " Table expressions
    DATA(ls_material) = lt_materials[ matnr = '10000001' ].
  ENDMETHOD.

  METHOD show_oo_example.
    " Class definition example
    CLASS lcl_material DEFINITION.
      PUBLIC SECTION.
        METHODS:
          constructor
            IMPORTING
              iv_matnr TYPE matnr,
          get_details
            RETURNING
              VALUE(rs_details) TYPE mara.
      PRIVATE SECTION.
        DATA: mv_matnr TYPE matnr.
    ENDCLASS.

    CLASS lcl_material IMPLEMENTATION.
      METHOD constructor.
        mv_matnr = iv_matnr.
      ENDMETHOD.

      METHOD get_details.
        SELECT SINGLE * FROM mara
          INTO rs_details
          WHERE matnr = mv_matnr.
      ENDMETHOD.
    ENDCLASS.
  ENDMETHOD.

  METHOD show_data_exchange.
    " OData service example
    CLASS lcl_odata_service DEFINITION.
      PUBLIC SECTION.
        METHODS:
          get_materials
            RETURNING
              VALUE(rt_materials) TYPE mara_t.
    ENDCLASS.

    CLASS lcl_odata_service IMPLEMENTATION.
      METHOD get_materials.
        SELECT * FROM mara
          INTO TABLE rt_materials
          UP TO 100 ROWS.
      ENDMETHOD.
    ENDCLASS.

    " IDOC processing example
    CLASS lcl_idoc_processor DEFINITION.
      PUBLIC SECTION.
        METHODS:
          process_idoc
            IMPORTING
              iv_idoc_number TYPE edidc-docnum.
    ENDCLASS.

    CLASS lcl_idoc_processor IMPLEMENTATION.
      METHOD process_idoc.
        DATA: lt_idoc_data TYPE TABLE OF edidd.
        
        CALL FUNCTION 'IDOC_READ_COMPLETELY'
          EXPORTING
            document_number         = iv_idoc_number
          TABLES
            int_edidd              = lt_idoc_data
          EXCEPTIONS
            document_not_exist     = 1
            document_number_invalid = 2
            OTHERS                 = 3.
      ENDMETHOD.
    ENDCLASS.
  ENDMETHOD.

  METHOD show_form_processing.
    " Adobe Form example
    CLASS lcl_adobe_form DEFINITION.
      PUBLIC SECTION.
        METHODS:
          generate_pdf
            IMPORTING
              iv_document_number TYPE vbak-vbeln.
    ENDCLASS.

    CLASS lcl_adobe_form IMPLEMENTATION.
      METHOD generate_pdf.
        DATA: lv_fm_name TYPE funcname.
        
        lv_fm_name = 'ZFM_SALES_ORDER_PDF'.
        
        CALL FUNCTION lv_fm_name
          EXPORTING
            iv_vbeln = iv_document_number
          EXCEPTIONS
            OTHERS   = 1.
      ENDMETHOD.
    ENDCLASS.

    " SAPScript form example
    CLASS lcl_sapscript_form DEFINITION.
      PUBLIC SECTION.
        METHODS:
          print_form
            IMPORTING
              iv_form_name TYPE tdsfname.
    ENDCLASS.

    CLASS lcl_sapscript_form IMPLEMENTATION.
      METHOD print_form.
        CALL FUNCTION 'OPEN_FORM'
          EXPORTING
            form      = iv_form_name
          EXCEPTIONS
            OTHERS    = 1.
      ENDMETHOD.
    ENDCLASS.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 2. Business Process Extension Examples
*----------------------------------------------------------------------*
CLASS lcl_business_process DEFINITION.
  PUBLIC SECTION.
    METHODS:
      enhance_sales_order
        IMPORTING
          iv_vbeln TYPE vbak-vbeln.
ENDCLASS.

CLASS lcl_business_process IMPLEMENTATION.
  METHOD enhance_sales_order.
    " BAdI implementation example
    DATA: lo_badi TYPE REF TO zif_sales_order_badi.
    
    GET BADI lo_badi.
    
    CALL BADI lo_badi->before_save
      EXPORTING
        iv_vbeln = iv_vbeln.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 3. Module-Specific Examples
*----------------------------------------------------------------------*
CLASS lcl_module_examples DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " FICO example
      process_accounting_document
        IMPORTING
          iv_belnr TYPE bkpf-belnr,
      " HR example
      process_employee_data
        IMPORTING
          iv_pernr TYPE pernr_d,
      " SD example
      process_sales_order
        IMPORTING
          iv_vbeln TYPE vbak-vbeln,
      " MM example
      process_purchase_order
        IMPORTING
          iv_ebeln TYPE ekko-ebeln.
ENDCLASS.

CLASS lcl_module_examples IMPLEMENTATION.
  METHOD process_accounting_document.
    " FICO document processing
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = VALUE bapiache09( )
      TABLES
        accountgl     = VALUE bapiacgl09_t( )
        currencyamount = VALUE bapiaccr09_t( ).
  ENDMETHOD.

  METHOD process_employee_data.
    " HR employee data processing
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr         = iv_pernr
        infty         = '0001'
      TABLES
        infty_tab     = DATA(lt_p0001).
  ENDMETHOD.

  METHOD process_sales_order.
    " SD sales order processing
    CALL FUNCTION 'BAPI_SALESORDER_CREATE'
      EXPORTING
        sales_header_in = VALUE bapisdhd1( )
      TABLES
        sales_items_in  = VALUE bapisditm_t( ).
  ENDMETHOD.

  METHOD process_purchase_order.
    " MM purchase order processing
    CALL FUNCTION 'BAPI_PO_CREATE'
      EXPORTING
        po_header     = VALUE bapimepoheader( )
      TABLES
        po_items      = VALUE bapimepoitem_t( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 4. Error Handling Examples
*----------------------------------------------------------------------*
CLASS lcl_error_handling DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_errors
        IMPORTING
          iv_error_code TYPE sy-subrc.
ENDCLASS.

CLASS lcl_error_handling IMPLEMENTATION.
  METHOD handle_errors.
    CASE iv_error_code.
      WHEN 1.
        MESSAGE e000(zz) WITH 'Error occurred'.
      WHEN 2.
        MESSAGE w000(zz) WITH 'Warning occurred'.
      WHEN OTHERS.
        MESSAGE i000(zz) WITH 'Information message'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* 5. Performance Optimization Examples
*----------------------------------------------------------------------*
CLASS lcl_performance DEFINITION.
  PUBLIC SECTION.
    METHODS:
      optimize_select
        IMPORTING
          iv_matnr TYPE matnr.
ENDCLASS.

CLASS lcl_performance IMPLEMENTATION.
  METHOD optimize_select.
    " Using SELECT with proper fields
    SELECT SINGLE matnr, maktx, mtart
      FROM mara
      INTO @DATA(ls_mara)
      WHERE matnr = @iv_matnr.

    " Using internal tables efficiently
    DATA: lt_materials TYPE TABLE OF mara.
    
    SELECT * FROM mara
      INTO TABLE lt_materials
      WHERE mtart IN @DATA(lt_mtart)
      ORDER BY matnr.
  ENDMETHOD.
ENDCLASS. 