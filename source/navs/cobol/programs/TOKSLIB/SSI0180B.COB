****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　片倉工業支払照合　　　　　　　　　*
*    モジュール名　　　　：　支払明細ファイル作成　　　　　　　*
*    作成日／更新日　　　：　95/11/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＣＶＣＳ１２８ファイルより支払明細*
*                        ：　ファイルを作成する。　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0180B.
 AUTHOR.                N.T.
 DATE-WRITTEN.          95/11/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＣＶＣＳ１２８ >>--*
     SELECT   CVCS128   ASSIGN         DA-01-S-CVCS128
                        ORGANIZATION   SEQUENTIAL
                        STATUS         CVC-ST.
*----<< 支払明細ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＣＶＣＳ１２８ >>--*
 FD  CVCS128            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
 01  CVC-REC.
     03  CVC-F01                 PIC  X(01).
     03  CVC-F02                 PIC  X(01).
     03  CVC-F03                 PIC  X(126).
*----<< 支払明細ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFC   OF        XFDLIB
              JOINING   SHI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  IDX.
     03  IX             PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  CVC-ST             PIC  X(02).
 01  SHI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  WRK-TEN.
     03  FILLER         PIC  X(02)     VALUE  "00".
     03  WRK-TENNO      PIC  X(03).
*----<< ﾍｯﾀﾞｰ ﾚｺｰﾄﾞ >>--*
 01  HED-REC.
     03  HED-F01        PIC  X(01).
     03  HED-F02        PIC  X(01).
     03  HED-F03        PIC  X(15).
     03  HED-F04        PIC  X(15).
     03  HED-F05        PIC  X(01).
     03  HED-F06        PIC  X(06).
     03  HED-F07        PIC  X(06).
     03  HED-F08        PIC  X(06).
     03  HED-F09        PIC  X(77).
*----<< ﾃﾞｰﾀ ﾚｺｰﾄﾞ >>--*
 01  MEI-REC.
     03  MEI-F01        PIC  X(01).
     03  MEI-F09        PIC  X(01).
     03  MEI-DATA                      OCCURS  3.
       05  MEI-F10      PIC  X(03).
       05  MEI-F11      PIC  X(02).
       05  MEI-F12      PIC  X(06).
       05  MEI-F13      PIC  X(06).
       05  MEI-F14      PIC  X(06).
       05  MEI-F15      PIC  X(01).
       05  MEI-F16      PIC  9(10).
       05  MEI-F17      PIC  X(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 CVC-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCS128.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0180B CVCS128 ERROR " CVC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    CVCS128   HSHIGKF.
     STOP     RUN.
*----<< 支払明細ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0180B HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    CVCS128   HSHIGKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  NOT =  SPACE.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0180B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     CVCS128.
     OPEN     OUTPUT    HSHIGKF.
*
     PERFORM  900-CVC-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     IF       CVC-F02        =   "H"
              MOVE      CVC-REC       TO   HED-REC
     ELSE
              MOVE      CVC-REC       TO   MEI-REC
              PERFORM   210-HENSYU-RTN
                        VARYING  IX  FROM  1  BY  1
                        UNTIL    IX  >     3
     END-IF
*
     PERFORM  900-CVC-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    CVCS128.
     CLOSE    HSHIGKF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0180B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     支払明細ファイル編集＋出力                   *
*--------------------------------------------------------------*
 210-HENSYU-RTN         SECTION.
     IF       MEI-F10(IX)    =   SPACE
         AND  MEI-F16(IX)    =   ZERO
              GO                 TO   210-HENSYU-RTN-EXIT
     END-IF.
*
     MOVE     SPACE              TO   SHI-REC.
     INITIALIZE                       SHI-REC.
*
     MOVE     HED-F03            TO   SHI-F10.
     MOVE     HED-F06            TO   SHI-F11.
     MOVE     HED-F07            TO   SHI-F12.
     MOVE     "0"                TO   SHI-F13.
     MOVE     "ｼﾖｳﾋﾝﾀﾞｲｷﾝ"       TO   SHI-F14.
     MOVE     HED-F08            TO   SHI-F32.
*
     MOVE     MEI-F11(IX)        TO   SHI-F17.
     MOVE     MEI-F10(IX)        TO   WRK-TENNO.
     MOVE     WRK-TEN            TO   SHI-F19.
     MOVE     MEI-F13(IX)        TO   SHI-F20.
     MOVE     MEI-F14(IX)        TO   SHI-F21.
     IF       MEI-F15(IX)    =   SPACE
              MOVE     MEI-F16(IX)    TO   SHI-F22
     ELSE
              COMPUTE  SHI-F22   =    MEI-F16(IX)  *  -1
     END-IF.
     MOVE     MEI-F17(IX)        TO   SHI-F23.
*
     WRITE    SHI-REC.
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＣＶＣＳ１２８　 READ                        *
*--------------------------------------------------------------*
 900-CVC-READ           SECTION.
     READ     CVCS128
         AT END
              MOVE      "END"         TO   END-FLG
     END-READ.
 900-CVC-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
