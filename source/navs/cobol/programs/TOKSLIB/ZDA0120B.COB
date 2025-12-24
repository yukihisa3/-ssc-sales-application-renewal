****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　仕入計上　　　　　　              *
*    作成日／更新日　　　：　93/05/12                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　仕入データを出力する　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZDA0120B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K6500.
 OBJECT-COMPUTER.       FACOM-K6500.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル >>*************************************
     SELECT   NYUK      ASSIGN    TO        DA-01-VI-ZNYUKDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYU-F01
                                                 NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
****<< 発注ファイル >>*************************************
     SELECT   HACH      ASSIGN    TO        DA-01-VI-ZHACHDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HA0-F01
                                                 HA0-F02
                                                 HA0-F03
                                                 HA0-F04
                                                 HA0-F05
                        FILE      STATUS    IS   HAC-STATUS.
****<< 商品変換テーブル >>**********************************
     SELECT   SHO       ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
****<< 取引先マスタ >>**************************************
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TMS-F01
                        FILE      STATUS    IS   TMS-STATUS.
****<< 仕入データ >>****************************************
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        FILE      STATUS    IS   TOK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入庫ファイル >>*************************************
 FD  NYUK.
     COPY     ZNYUKDT   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 発注ファイル >>*************************************
 FD  HACH.
     COPY     ZHACHDT.
****<< 商品変換テーブル >>**********************************
 FD  SHO.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
****<< 取引先マスタ >>**********************************
 FD  TOKMS2.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TMS       PREFIX.
****<< 商品在庫マスタファイル >>****************************
 FD  TOKU.
     COPY    TOKU.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYU-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
     02 SHO-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 TMS-STATUS           PIC  X(02).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  ワークエリア            ****
 01  HOZON-AREA.
     03  HZN-CODE            PIC  X(08).
*## 1999/12/20 NAV
*****03  HZN-F23             PIC  9(06).
     03  HZN-F23             PIC  9(08).
     03  HZN-F09             PIC  X(02).
*
 01  KEY-AREA.
     03  NEW-KEY.
         05  NEW-01          PIC  9(02).
         05  NEW-02          PIC  9(07).
         05  NEW-03          PIC  9(02).
         05  NEW-04          PIC  9(01).
     03  OLD-KEY.
         05  OLD-01          PIC  9(02).
         05  OLD-02          PIC  9(07).
         05  OLD-03          PIC  9(02).
         05  OLD-04          PIC  9(01).
 01  HAC-INV                 PIC  9(01)  VALUE ZERO.
 01  SORYO-FLG               PIC  9(01)  VALUE ZERO.
* ＪＥＦ－ＪＩＳ　変換用　引数
 01  JEIC-RTNCD             PIC  S9(09)  BINARY.
 01  JEIC-INLT              PIC  S9(04)  BINARY VALUE 36.
 01  JEIC-INDT              PIC   X(36).
 01  JEIC-OTLT              PIC  S9(04)  BINARY VALUE 36.
 01  JEIC-OTDT              PIC   X(36).
 01  JEIC-DTLT              PIC  S9(04)  BINARY.
 01  WK-JEF                 PIC   N(18).
 01  WK-X   REDEFINES       WK-JEF.
     03   WK-XX             PIC   X(36).
 01  WK-NYU-F02             PIC   9(07)  VALUE  ZERO.
*
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  日付保存                ****
 01  SYSTEM-HIZUKE2.
     02  SYSYMD2             PIC  9(8).
     02  SYSYMD2-R           REDEFINES SYSYMD2.
       03  SYS-YY2           PIC  9999.
       03  SYS-MM2           PIC  99.
       03  SYS-DD2           PIC  99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZDA0120B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYUK.
     MOVE   "ZNYUKDT "        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACH.
     MOVE   "ZHACHDT "        TO    ERR-FL-ID.
     MOVE    HAC-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SHO.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    SHO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKMS2.
     MOVE   "TOKMS2  "        TO    ERR-FL-ID.
     MOVE    TMS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKU.
     MOVE   "TOKU    "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
                            UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
     OPEN         I-O       NYUK.
     OPEN         I-O       HACH.
     OPEN         INPUT     SHO.
     OPEN         INPUT     TOKMS2.
     OPEN         EXTEND    TOKU.
*       システム日付の取得
     ACCEPT       SYSYMD    FROM     DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYSYMD2.
     DISPLAY "SYSYMD2 = " SYSYMD2 UPON CONS.
     MOVE         LOW-VALUE      TO  NEW-KEY
                                     OLD-KEY.
*       入庫ファイルの読込
     PERFORM           NYUK-READ.
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
     MOVE    NEW-KEY             TO   OLD-KEY.
     IF      NEW-01    =    60   OR
             NEW-01    =    61
             MOVE      NYU-F08   TO   HZN-CODE
     ELSE
             MOVE      NYU-F07   TO   HZN-CODE
     END-IF.
*## 1999/12/20 NAV
*****MOVE    NYU-F23(3:6)        TO   HZN-F23.
     MOVE    NYU-F23             TO   HZN-F23.
*
     MOVE         ZERO           TO   SORYO-FLG.
     PERFORM      210-HENSYU1
        UNTIL     NEW-KEY   NOT  =    OLD-KEY.
*
     PERFORM      220-HENSYU2.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*      210       仕入データ１の編集                        *
************************************************************
 210-HENSYU1                     SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
*    部門
*## 1999/12/20 NAV
*****MOVE    290                 TO   TO0-F01.
     MOVE    2900                TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    NYU-F02             TO   TO0-F03(1:7).
     MOVE    NYU-F03             TO   TO0-F03(8:2).
*    行_
     MOVE    NYU-F05             TO   TO0-F04.
*    赤黒区分
     MOVE    NYU-F04             TO   TO0-F05.
*   得意先／仕入先コード
**** MOVE    HZN-CODE            TO   TO0-F06.
     IF      NYU-F01        =    50   OR   51   OR   54   OR  55
                                      OR   52
             MOVE NYU-F07   TO   TO0-F06
     ELSE
*
*   取引先マスタＲＥＡＤ（自社得意先コード取得）
*****   IF   HZN-CODE(1:5)    =    "99999"
*****        MOVE HZN-CODE    TO   TO0-F06
*****   ELSE
*****        PERFORM  TMS-READ
******  END-IF
        IF   NYU-F08(1:5)     =    "99999"
             MOVE     NYU-F08 TO   TO0-F06
        ELSE
             PERFORM  TMS-READ
        END-IF
     END-IF.
***
*   納入先コード
     MOVE    NYU-F12             TO   TO0-F07.
*   発注日
*## 1999/12/20 NAV
*****MOVE    NYU-F29(3:6)        TO   TO0-F08.
     MOVE    NYU-F29             TO   TO0-F08.
*   納品日、出荷日
     MOVE    HZN-F23             TO   TO0-F09
                                      TO0-F10.
*   商品コード
     MOVE    NYU-F13             TO   TO0-F11.
*   品単
     MOVE    NYU-F14             TO   TO0-F12.
*   単価区分
     MOVE    NYU-F16             TO   TO0-F14.
*   数量
     MOVE    NYU-F15             TO   TO0-F15.
*   単価
     MOVE    NYU-F17             TO   TO0-F16.
*   金額
     IF      NYU-F16             =    3
             MOVE      ZERO      TO   TO0-F16
             MOVE      NYU-F17   TO   TO0-F17
     ELSE
             COMPUTE   TO0-F17   =    NYU-F15  *  NYU-F17
     END-IF.
*\\  93.06.15  START \\
     IF      NYU-F01             =    60  OR 61
             MOVE      NYU-F18   TO   TO0-F14
             MOVE      NYU-F19   TO   TO0-F16
         IF  TO0-F14              =   "3"
             MOVE      NYU-F19   TO   TO0-F17
             MOVE      ZERO      TO   TO0-F16
         ELSE
             COMPUTE   TO0-F17   =    NYU-F15 * NYU-F19
         END-IF
     END-IF.
*\\
*   場所
     MOVE    NYU-F24             TO   TO0-F18.
     IF      NYU-F01             =    60  OR 61
             MOVE      "79"      TO   TO0-F18
     ELSE
        IF   NYU-F01             =    70  OR 71  OR  80  OR  81
         IF  NYU-F07    NOT =         "29099999"
*************                         "12345678"
             MOVE      "79"      TO   TO0-F18
          END-IF
        END-IF
     END-IF.
*
*   入出庫区分
     MOVE    ZERO                TO   TO0-F19.
*   税区分
     MOVE    NYU-F09             TO   TO0-F20.
*   備考
     MOVE    NYU-F21             TO   TO0-F22.
*
     IF      NYU-F01        =    70  OR  71  OR  80  OR  81
             MOVE    NYU-F12(4:2)        TO   TO0-F21
     END-IF.
*
*   注文_
     MOVE    NYU-F01             TO   HA0-F01.
     MOVE    NYU-F02             TO   HA0-F02.
     MOVE    NYU-F03             TO   HA0-F03.
     MOVE    NYU-F04             TO   HA0-F04.
     MOVE    NYU-F05             TO   HA0-F05.
     PERFORM      HACH-READ.
     MOVE    HAC-F18             TO   TO0-F23.
*   量販店商品コード
     PERFORM      SHO-READ.
     MOVE    SHO-F02             TO   TO0-F24.
*   担当者
     MOVE    HAC-F09             TO   TO0-F25.
*   請求区分
     MOVE    NYU-F25             TO   TO0-F26.
*   処理日
*## 1999/12/20 NAV
*****MOVE    SYSYMD              TO   TO0-F27.
     MOVE    SYSYMD2             TO   TO0-F27.
*   直送
***  IF      NYU-F01   =    60   OR  61  OR  50  OR  51
***          MOVE      NYU-F07   TO   TO0-F28
***          MOVE      NYU-F16   TO   TO0-F29
***     IF   NYU-F16   =    3
***          COMPUTE   TO0-F31   =    NYU-F15   *    NYU-F17
***     ELSE
***          MOVE      NYU-F17   TO   TO0-F30
***                                   TO0-F31
***     END-IF
***  END-IF.
*
     MOVE    NYU-F07             TO   TO0-F28.
*****IF      NYU-F01        NOT =     60  AND 61
*
     MOVE    NYU-F16             TO   TO0-F29.
     MOVE    NYU-F17             TO   TO0-F30.
     IF      NYU-F16     =    3
             MOVE      NYU-F17   TO   TO0-F31
             MOVE      ZERO      TO   TO0-F30
     ELSE
             COMPUTE   TO0-F31   =
                           NYU-F15  *  NYU-F17
     END-IF.
*
*****ELSE
*****        MOVE    NYU-F18     TO   TO0-F29
*****        MOVE    NYU-F19     TO   TO0-F30
*****        IF      NYU-F18     =    3
*****                MOVE      NYU-F19   TO   TO0-F31
*****        ELSE
*****            COMPUTE   TO0-F31   =
*****                      NYU-F15  *  NYU-F19
*****        END-IF
*****END-IF.
*   __
     MOVE    NYU-F22             TO   TO0-F32.
*   量販_
     MOVE    NYU-F06             TO   TO0-F33.
     IF      NYU-F15        =    ZERO
             GO TO               210-HENSYU1-99
     ELSE
******DISPLAY "TO0-F271= " TO0-F27 UPON CONS
             WRITE               TOK-REC
     END-IF.
*
     IF      SORYO-FLG      =    ZERO
        IF   NYU-F10        =    8  OR  9
                  PERFORM             SORYO-SEC
        END-IF
     END-IF.
*
 210-HENSYU1-99.
*****MOVE    1                   TO   NYU-F27.
     MOVE    1                   TO   NYU-F28.
     REWRITE                     NYU-REC.
*
     MOVE    1                   TO   HAC-F35.
     IF      HAC-INV   =              ZERO
             REWRITE                  HAC-REC
     END-IF.
*
     PERFORM      NYUK-READ.
 210-HENSYU1-EXT.
     EXIT.
************************************************************
*                送料データの作成                          *
************************************************************
 SORYO-SEC                       SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
*    部門
*## 1999/12/20 NAV
*****MOVE    290                 TO   TO0-F01.
     MOVE    2900                TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    NYU-F02             TO   TO0-F03(1:7).
     MOVE    NYU-F03             TO   TO0-F03(8:2).
*    行_
     MOVE    90                  TO   TO0-F04.
*    赤黒区分
     MOVE    NYU-F04             TO   TO0-F05.
*   取引先マスタＲＥＡＤ（自社得意先コード取得）
     PERFORM  TMS-READ.
     MOVE    NYU-F08         TO   TMS-F01.
     READ    TOKMS2
       INVALID      KEY
             MOVE  SPACE          TO   TO0-F06
       NOT   INVALID      KEY
             MOVE  TMS-F52        TO   TO0-F06
     END-READ.
*
     IF      NYU-F01      =       50 OR 51
             MOVE  NYU-F07        TO   TO0-F06
             MOVE    "3"          TO   TO0-F14
             MOVE  ZERO           TO   TO0-F16
             MOVE  NYU-F11        TO   TO0-F17
     END-IF.
***
*   納入先コード
     MOVE    NYU-F12             TO   TO0-F07.
*   発注日
*## 1999/12/20 NAV
*****MOVE    NYU-F29(3:6)        TO   TO0-F08.
     MOVE    NYU-F29             TO   TO0-F08.
*   納品日、出荷日
     MOVE    HZN-F23             TO   TO0-F09
                                      TO0-F10.
*   金額
*****MOVE    NYU-F11             TO   TO0-F17.
**** IF      NYU-F10             =    9
****         MOVE    NYU-F11     TO   TO0-F31
**** END-IF.
*   場所
     MOVE    NYU-F24             TO   TO0-F18.
*   入出庫区分
     MOVE    ZERO                TO   TO0-F19.
*   税区分
     MOVE    NYU-F09             TO   TO0-F20.
*   備考
     MOVE    NYU-F21             TO   TO0-F22.
*   注文_
     MOVE    NYU-F01             TO   HA0-F01.
     MOVE    NYU-F02             TO   HA0-F02.
     MOVE    NYU-F03             TO   HA0-F03.
     MOVE    NYU-F04             TO   HA0-F04.
     MOVE    NYU-F05             TO   HA0-F05.
     PERFORM      HACH-READ.
     MOVE    HAC-F18             TO   TO0-F23.
*   担当者
     MOVE    HAC-F09             TO   TO0-F25.
*   請求区分
     MOVE    NYU-F25             TO   TO0-F26.
*   処理日
*## 1999/12/20 NAV
*****MOVE    SYSYMD              TO   TO0-F27.
     MOVE    SYSYMD2             TO   TO0-F27.
*
     EVALUATE     NYU-F01
             WHEN      60
             WHEN      61
             WHEN      50
             WHEN      51
             WHEN      55
             WHEN      56
                  IF   NYU-F10   =    8
                       MOVE      "00999960"
                                 TO   TO0-F11
                  ELSE
                       MOVE      "00999961"
                                 TO   TO0-F11
                  END-IF
             WHEN      52
                  MOVE "0099962" TO   TO0-F11
     END-EVALUATE.
**** IF      NYU-F01   =    60   OR   61
****         MOVE    NYU-F11     TO   TO0-F31
****     IF      NYU-F10   =         9
****             MOVE      NYU-F11   TO   TO0-F17
****     END-IF
**** ELSE
****         MOVE    NYU-F11     TO   TO0-F17
****     IF      NYU-F10   =         9
****             MOVE      NYU-F11   TO   TO0-F31
****     END-IF
**** END-IF.
*
     MOVE    "3"       TO        TO0-F29.
     MOVE    ZERO      TO        TO0-F30.
     MOVE    NYU-F11   TO        TO0-F31.
     MOVE    NYU-F07   TO        TO0-F28.
     IF      NYU-F10   =         9
             MOVE      3         TO        TO0-F14
             MOVE      ZERO      TO        TO0-F16
             MOVE      NYU-F11   TO        TO0-F17
     END-IF.
*
*
******DISPLAY "TO0-F272= " TO0-F27 UPON CONS.
     WRITE             TOK-REC.
*
     MOVE    1         TO        SORYO-FLG.
*
 SORYO-SEC-EXT.
     EXIT.
************************************************************
*      220       仕入データ２の編集                        *
************************************************************
 220-HENSYU2                     SECTION.
     MOVE    SPACE               TO   TOK1-REC.
     INITIALIZE                       TOK1-REC.
*    部門
*## 1999/12/20 NAV
*****MOVE    290                 TO   TO1-F01.
     MOVE    2900                TO   TO1-F01.
*    伝票区分
     MOVE    OLD-01              TO   TO1-F02.
*    伝票_
     MOVE    OLD-02              TO   TO1-F03(1:7).
     MOVE    OLD-03              TO   TO1-F03(8:2).
*    行_
     MOVE    ZERO                TO   TO1-F04.
*    赤黒区分
     MOVE    OLD-04              TO   TO1-F05.
*   得意先／仕入先コード
**** MOVE    HZN-CODE            TO   TO0-F06.
*   取引先マスタＲＥＡＤ（自社得意先コード取得）
     PERFORM  TMS-READ.
****
*    335.1名
     MOVE    OLD-01              TO   HA0-F01.
     MOVE    OLD-02              TO   HA0-F02.
     MOVE    OLD-03              TO   HA0-F03.
     MOVE    OLD-04              TO   HA0-F04.
     MOVE    ZERO                TO   HA0-F05.
     MOVE    ZERO                TO   HAC-INV.
*
*
     PERFORM      HACH-READ.
     DISPLAY HAC-INV UPON CONS.
     IF      HAC-INV   NOT =     ZERO
             GO TO
                                 220-HENSYU2-EXT
     END-IF.
*****MOVE    HA0-F08             TO   TO1-F07.
     MOVE    HA0-F08             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F07.
*
*    住所１
*****MOVE    HA0-F09             TO   TO1-F08.
     MOVE    HA0-F09             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F08.
*    住所２
*****MOVE    HA0-F10             TO   TO1-F09.
     MOVE    HA0-F10             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F09.
*    郵便_
     MOVE    HA0-F07             TO   TO1-F12.
*    担当者
     MOVE    OLD-01              TO   HA0-F01.
     MOVE    OLD-02              TO   HA0-F02.
     MOVE    OLD-03              TO   HA0-F03.
     MOVE    OLD-04              TO   HA0-F04.
     MOVE    01                  TO   HA0-F05.
     PERFORM      HACH-READ.
     MOVE    HAC-F09             TO   TO1-F10.
*    処理日
*## 1999/12/20 NAV
*****MOVE    SYSYMD              TO   TO1-F11.
     MOVE    SYSYMD2             TO   TO1-F11.
*    出荷日
     MOVE    HZN-F23             TO   TO1-F13.
******DISPLAY "TO1-F11 = " TO1-F11 UPON CONS.
     WRITE                       TOK-REC.
 220-HENSYU2-EXT.
     EXIT.
************************************************************
*      9000      入庫ファイルの読込処理                    *
************************************************************
 NYUK-READ              SECTION.
     READ    NYUK      NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             MOVE      HIGH-VALUE     TO   NEW-KEY
             GO        TO        NYUK-READ-EXT
     END-READ.
*
     IF      (NYU-F27  NOT  =    1)   OR
             (NYU-F28  NOT  =    ZERO)
             GO        TO        NYUK-READ
     END-IF.
     MOVE    NYU-F01             TO   NEW-01.
     MOVE    NYU-F02             TO   NEW-02.
     MOVE    NYU-F03             TO   NEW-03.
     MOVE    NYU-F04             TO   NEW-04.
 NYUK-READ-EXT.
     EXIT.
************************************************************
*      発注ファイルの読込
************************************************************
 HACH-READ              SECTION.
     READ    HACH
       INVALID      KEY
          INITIALIZE              HAC-REC
          MOVE      9       TO    HAC-INV
       NOT INVALID      KEY
          MOVE      ZERO    TO    HAC-INV
     END-READ.
 HACH-READ-EXT.
     EXIT.
************************************************************
*      商品コード変換テーブルの読込                      *
************************************************************
 SHO-READ               SECTION.
     MOVE    NYU-F08         TO   SHO-F01.
     MOVE    NYU-F13         TO   SHO-F031.
     MOVE    NYU-F14         TO   SHO-F032.
     READ    SHO
       INVALID      KEY
          INITIALIZE              SHO-REC
     END-READ.
 SHO-READ-EXT.
     EXIT.
************************************************************
*      取引先マスタの読込                                  *
************************************************************
 TMS-READ               SECTION.
     MOVE    HZN-CODE        TO   TMS-F01.
     READ    TOKMS2
       INVALID      KEY
             MOVE  SPACE          TO   TO0-F06
       NOT   INVALID      KEY
             MOVE  TMS-F52        TO   TO0-F06
     END-READ.
 TMS-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC            SECTION.
     CLOSE        NYUK      HACH      SHO  TOKU  TOKMS2.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
