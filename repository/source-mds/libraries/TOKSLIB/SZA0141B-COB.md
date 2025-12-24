# SZA0141B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZA0141B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　仕入計上　　　　　　              *
*    作成日／更新日　　　：　2000/05/19                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    作成日／更新日　　　：　2006/01/20                        *
*    作成者／更新者　　　：　NAV T.MATSUNO                     *
*    作成日／更新日　　　：　2008/08/20                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI 内部統制対応    *
*    作成日／更新日　　　：　2011/11/15                        *
*    作成者／更新者　　　：　NAV T.MIURA 基幹サーバ統合*
*    作成日／更新日　　　：　2013/12/25 *##                    *
*    作成者／更新者　　　：　NAV T.TAKAHASHI 消費税増税対応*
*    　　変更概要　　　　：　税区分のセット方法を変更　　　*
*    処理概要　　　　　　：　仕入データを出力する　　　　　　　*
*    更新概要　　　　　　：　部門コードを仕入先コードより取得　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0141B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル >>*************************************
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
****<< 発注ファイル >>*************************************
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HAH-F02
                        FILE      STATUS    IS   HAH-STATUS.
****<< 発注ファイル >>*************************************
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HAM-F02
                                                 HAM-F03
                        FILE      STATUS    IS   HAM-STATUS.
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
****<< 実績用仕入データ >>**********************************
     SELECT   TOKUJ     ASSIGN    TO        DA-01-S-TOKUJ
                        FILE      STATUS    IS   TOKJ-STATUS.
****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入庫ファイル >>*************************************
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 発注ファイル >>*************************************
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HAH       PREFIX.
****<< 発注ファイル >>*************************************
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   HAM       PREFIX.
****<< 商品変換テーブル >>**********************************
 FD  SHO.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
****<< 取引先マスタ >>**************************************
 FD  TOKMS2.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TMS       PREFIX.
****<< 仕入データ >>****************************************
 FD  TOKU.
     COPY    TOKU.
****<< 実績用仕入データ >>**********************************
 FD  TOKUJ.
     COPY    TOKUJ.
****<< 条件ファイル >>**************************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*##2013/12/25 NAV ST 消費税増税対応
*条件ファイル格納領域
     COPY   HJYOKEN  OF XFDLIB  JOINING   JBK  AS   PREFIX.
*##2013/12/25 NAV ED
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYU-STATUS           PIC  X(02).
     02 HAH-STATUS           PIC  X(02).
     02 HAM-STATUS           PIC  X(02).
     02 SHO-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 TOKJ-STATUS          PIC  X(02).
     02 TMS-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  フラグ                  ****
 01  WK-BUMON                PIC  9(04)  VALUE  ZERO.
****  ワークエリア            ****
 01  HOZON-AREA.
     03  HZN-CODE            PIC  X(08).
*## 1999/12/20 NAV
*****03  HZN-F26             PIC  9(06).
     03  HZN-F26             PIC  9(08).
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
 01  HAM-INV                 PIC  9(01)  VALUE ZERO.
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
 01  WK-SOKCD               PIC   X(02)  VALUE  SPACE.
 01  WK-BUMON-CD            PIC   9(04)  VALUE  ZERO.
 01  WK-BUMON-CD1           PIC   9(04)  VALUE  ZERO.
 01  WK-BUMON-CD2           PIC   9(04)  VALUE  ZERO.
 01  HJYOKEN-INV-FLG        PIC   X(03)  VALUE  SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0141B".
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
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILF "        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACHEDF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    HAH-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACMEIF "        TO    ERR-FL-ID.
     MOVE    HAM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SHO.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    SHO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKMS2.
     MOVE   "TOKMS2  "        TO    ERR-FL-ID.
     MOVE    TMS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKU.
     MOVE   "TOKU    "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC8           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKUJ.
     MOVE   "TOKUJ   "        TO    ERR-FL-ID.
     MOVE    TOKJ-STATUS       TO    ERR-STCD.
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
     OPEN         I-O       NYKFILF.
     OPEN         I-O       HACHEDF HACMEIF.
     OPEN         INPUT     SHO.
     OPEN         INPUT     TOKMS2.
     OPEN         INPUT     HJYOKEN.
     OPEN         EXTEND    TOKU TOKUJ.
*    条件ファイル索引（部門コード取得）
*****　更新日を取得する　****
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
     END-READ.
*##2013/12/25 NAV ST 消費税増税対応（消費税率レコード取得）
     MOVE    99              TO   JYO-F01.
     MOVE    "ZEI"           TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-REC     TO   JBK-REC
     END-READ.
     DISPLAY "#" NC"税率１　　　　　" " = " JBK-F04   UPON CONS.
     DISPLAY "#" NC"税率２　　　　　" " = " JBK-F05   UPON CONS.
     DISPLAY "#" NC"税率改定日　　　" " = " JBK-F06   UPON CONS.
     DISPLAY "#" NC"改定後税率１　　" " = " JBK-F07   UPON CONS.
     DISPLAY "#" NC"改定後税率２　　" " = " JBK-F08   UPON CONS.
     DISPLAY "#" NC"計上税区分　　　" " = " JBK-F14   UPON CONS.
     DISPLAY "#" NC"改定後計上税区分" " = " JBK-F15   UPON CONS
*##2013/12/25 NAV ED
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
*    入庫ファイルの読込
     PERFORM           NYUK-READ.
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*    入庫ファイルキー入替え
     MOVE    NEW-KEY             TO   OLD-KEY.
*    伝票区分＝６０、６１の時
     IF      NEW-01    =    60   OR
             NEW-01    =    61
*            取引先ＣＤ
             MOVE      NYU-F09   TO   HZN-CODE
     ELSE
*            仕入先ＣＤ
             MOVE      NYU-F08   TO   HZN-CODE
     END-IF.
*    納入日
     MOVE    NYU-F26             TO   HZN-F26.
*    送料フラグ初期化
     MOVE    ZERO                TO   SORYO-FLG.
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
     MOVE    SPACE               TO   TOKJ0-REC.
     INITIALIZE                       TOKJ0-REC.
*    部門
*****MOVE    WK-BUMON            TO   TO0-F01.
*****出庫場所部門取得
*****MOVE    NYU-F27             TO   WK-SOKCD.
*****PERFORM HJYOKEN-READ-SEC.
*****IF      HJYOKEN-INV-FLG = "INV"
*************GO                  TO   210-HENSYU1-010
*****ELSE
*************MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
*****END-IF.
     EVALUATE     NYU-F08(1:3)
         WHEN     291
                  MOVE   "2910"  TO   WK-BUMON-CD1
         WHEN     292
                  MOVE   "2920"  TO   WK-BUMON-CD1
         WHEN     293
                  MOVE   "2930"  TO   WK-BUMON-CD1
         WHEN     294
                  MOVE   "2940"  TO   WK-BUMON-CD1
         WHEN     OTHER
                  MOVE    NYU-F27     TO WK-SOKCD
                  PERFORM HJYOKEN-READ-SEC
                  IF      HJYOKEN-INV-FLG = "INV"
                          GO          TO 210-HENSYU1-010
                  ELSE
                          MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
                  END-IF
     END-EVALUATE.
*    部門
     MOVE    WK-BUMON-CD1        TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    NYU-F02             TO   TO0-F03(1:7).
     MOVE    NYU-F03             TO   TO0-F03(8:2).
*    行_
     MOVE    NYU-F05             TO   TO0-F04.
*    赤黒区分（相殺区分）
     MOVE    NYU-F04             TO   TO0-F05.
*   得意先／仕入先コード
     IF      NYU-F01        =    50   OR   51   OR   54   OR  55
                                      OR   52
             MOVE NYU-F08   TO   TO0-F06
     ELSE
*   取引先マスタＲＥＡＤ（自社得意先コード取得）
        IF   NYU-F09(1:5)     =    "99999"
             MOVE     NYU-F09 TO   TO0-F06
        ELSE
             PERFORM  TMS-READ
        END-IF
     END-IF.
*   納入先コード
     MOVE    NYU-F12             TO   TO0-F07.
*   発注日
     MOVE    NYU-F13             TO   TO0-F08.
*   納品日、出荷日
     MOVE    HZN-F26             TO   TO0-F09
                                      TO0-F10.
*   商品コード
     MOVE    NYU-F15             TO   TO0-F11.
*   品単
     MOVE    NYU-F16             TO   TO0-F12.
*   単価区分
     MOVE    NYU-F19             TO   TO0-F14.
*   数量
     MOVE    NYU-F18             TO   TO0-F15.
*   単価
     MOVE    NYU-F20             TO   TO0-F16.
*   金額
     IF      NYU-F19             =    3
             MOVE      ZERO      TO   TO0-F16
             MOVE      NYU-F20   TO   TO0-F17
     ELSE
             COMPUTE   TO0-F17   =    NYU-F18  *  NYU-F20
     END-IF.
*    伝区＝直送の場合
     IF      NYU-F01             =    60  OR 61
*            単価区分
             MOVE      NYU-F21   TO   TO0-F14
*            単価
             MOVE      NYU-F22   TO   TO0-F16
*        単価区分＝３の時
         IF  TO0-F14              =   "3"
*            単価を金額へセット
             MOVE      NYU-F22   TO   TO0-F17
*            単価に０セット
             MOVE      ZERO      TO   TO0-F16
         ELSE
*            金額＝入庫数＊原価単価
             COMPUTE   TO0-F17   =    NYU-F18 * NYU-F22
         END-IF
     END-IF.
*   場所
     MOVE    NYU-F27             TO   TO0-F18.
*   伝区＝６０、６１の時
     IF      NYU-F01             =    60  OR 61
             MOVE      "79"      TO   TO0-F18
     ELSE
*       伝区＝７０、７１、８０、８１の時
        IF   NYU-F01             =    70  OR 71  OR  80  OR  81
*        仕入先＝特販部の場合
         IF  NYU-F08    NOT =         "29099999"
             MOVE      "79"      TO   TO0-F18
          END-IF
        END-IF
     END-IF.
*   入出庫区分
     MOVE    ZERO                TO   TO0-F19.
*   税区分
*##2013/12/25 NAV ST 消費税増税対応　税区分セット方法変更
*****MOVE    NYU-F10             TO   TO0-F20.
*****DISPLAY "NYU-F10 = " NYU-F10  UPON CONS.
     IF      NYU-F10 = SPACE  OR  "0"
             IF   HZN-F26  >=  JBK-F06
                  MOVE   JBK-F15 TO   TO0-F20
             ELSE
                  MOVE   JBK-F14 TO   TO0-F20
             END-IF
     ELSE
             MOVE        NYU-F10 TO   TO0-F20
     END-IF.
*****DISPLAY "TO0-F20 = " TO0-F20  UPON CONS.
*##2013/12/25 NAV ED 消費税増税対応　税区分セット方法変更
*   備考
     MOVE    NYU-F24             TO   TO0-F22.
*   伝区＝７０、７１、８０、８１の場合＝＞納入先の下２桁セット
     IF      NYU-F01        =    70  OR  71  OR  80  OR  81
*            作業明細区分のセット
             MOVE    NYU-F12(4:2)        TO   TO0-F21
     END-IF.
*   発注ヘッダ読込み（キー＝発注番号）
     MOVE    NYU-F02             TO   HAH-F02.
     PERFORM HACH-READ.
*   発注明細読込み（キー＝発注番号＋行番号）
     MOVE    NYU-F02             TO   HAM-F02.
     MOVE    NYU-F05             TO   HAM-F03.
     PERFORM HACM-READ.
*    メモ番号
     MOVE    HAH-F15             TO   TO0-F23.
*   量販店商品コード
     PERFORM      SHO-READ.
     MOVE    SHO-F02             TO   TO0-F24.
*   担当者
*## 2008/08/20 内部統制対応 ST ##*
*****MOVE    HAH-F05             TO   TO0-F25.
     MOVE    NYU-F33             TO   TO0-F25.
*   請求区分
     MOVE    NYU-F28             TO   TO0-F26.
*   処理日
     MOVE    SYSYMD2             TO   TO0-F27.
*   仕入先
     MOVE    NYU-F08             TO   TO0-F28.
*   単価区分
     MOVE    NYU-F19             TO   TO0-F29.
*   仕入単価
     MOVE    NYU-F20             TO   TO0-F30.
*   単価区分＝３の場合
     IF      NYU-F19     =    3
*            仕入単価
             MOVE      NYU-F20   TO   TO0-F31
*            仕入金額
             MOVE      ZERO      TO   TO0-F30
     ELSE
*            仕入金額＝入庫数＊仕入単価
             COMPUTE   TO0-F31   =
                           NYU-F18  *  NYU-F20
     END-IF.
*   __
     MOVE    NYU-F17             TO   TO0-F32.
*   量販_
     MOVE    NYU-F07             TO   TO0-F33.
*   支払締日
     MOVE    NYU-F34             TO   TO0-F34.
*## 2000/09/13 START ##
     MOVE    TOK-REC             TO   TOKJ-REC(1:256).
*   入庫数＝０の場合、入庫ファイル更新へ
*   入庫数＝０以外の場合、計上ファイル更新へ
     IF      NYU-F18   =   ZERO
*            単価区分＝”３”の場合は、計上ファイル更新へ
             IF   NYU-F19  NOT = "3"
                  GO TO    210-HENSYU1-99
             ELSE
                  WRITE    TOK-REC
                  WRITE    TOKJ-REC
             END-IF
     ELSE
             WRITE         TOK-REC
             WRITE         TOKJ-REC
     END-IF.
*    送料フラグ
     IF      SORYO-FLG      =    ZERO
*       送料区分＝８、９の時
        IF   NYU-F111       =    8  OR  9
                  PERFORM             SORYO-SEC
        END-IF
     END-IF.
*
 210-HENSYU1-99.
*   計上フラグセット
     MOVE    1                   TO   NYU-F31.
*   入庫ファイル更新
     REWRITE                     NYU-REC.
*    発注明細計上区分更新
     MOVE    1                   TO   HAM-F20.
     IF      HAM-INV   =              ZERO
             REWRITE                  HAM-REC
     END-IF.
*
 210-HENSYU1-010.
     PERFORM      NYUK-READ.
 210-HENSYU1-EXT.
     EXIT.
************************************************************
*                送料データの作成                          *
************************************************************
 SORYO-SEC                       SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
     MOVE    SPACE               TO   TOKJ0-REC.
     INITIALIZE                       TOKJ0-REC.
*    部門
*****MOVE    WK-BUMON                TO   TO0-F01.
*****出庫場所部門取得
*****MOVE    NYU-F27             TO   WK-SOKCD.
*****PERFORM HJYOKEN-READ-SEC.
*****IF      HJYOKEN-INV-FLG = "INV"
*************GO                  TO   210-HENSYU1-010
*****ELSE
*************MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
*****END-IF.
     EVALUATE     NYU-F08(1:3)
         WHEN     291
                  MOVE   "2910"  TO   WK-BUMON-CD1
         WHEN     292
                  MOVE   "2920"  TO   WK-BUMON-CD1
         WHEN     293
                  MOVE   "2930"  TO   WK-BUMON-CD1
         WHEN     294
                  MOVE   "2940"  TO   WK-BUMON-CD1
         WHEN     OTHER
                  MOVE    NYU-F27     TO WK-SOKCD
                  PERFORM HJYOKEN-READ-SEC
                  IF      HJYOKEN-INV-FLG = "INV"
                          GO          TO 210-HENSYU1-010
                  ELSE
                          MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
                  END-IF
     END-EVALUATE.
*    部門
     MOVE    WK-BUMON-CD1        TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    NYU-F02             TO   TO0-F03(1:7).
     MOVE    NYU-F03             TO   TO0-F03(8:2).
*    行_
     MOVE    90                  TO   TO0-F04.
*    赤黒区分（相殺区分）
     MOVE    NYU-F04             TO   TO0-F05.
*   取引先マスタＲＥＡＤ（自社得意先コード取得）
     PERFORM  TMS-READ.
     MOVE    NYU-F09         TO   TMS-F01.
     READ    TOKMS2
       INVALID      KEY
             MOVE  SPACE          TO   TO0-F06
       NOT   INVALID      KEY
             MOVE  TMS-F52        TO   TO0-F06
     END-READ.
*    伝票区分＝５０、５１
     IF      NYU-F01      =       50 OR 51
*            仕入先ＣＤ
             MOVE  NYU-F08        TO   TO0-F06
*            単価区分
             MOVE    "3"          TO   TO0-F14
*            単価
             MOVE  ZERO           TO   TO0-F16
*            金額
             MOVE  NYU-F112       TO   TO0-F17
     END-IF.
*   納入先コード
     MOVE    NYU-F12             TO   TO0-F07.
*   発注日
     MOVE    NYU-F13             TO   TO0-F08.
*   納品日、出荷日
     MOVE    HZN-F26             TO   TO0-F09
                                      TO0-F10.
*   場所
     MOVE    NYU-F27             TO   TO0-F18.
*   入出庫区分
     MOVE    ZERO                TO   TO0-F19.
*   税区分
*##2013/12/25 NAV ST 消費税増税対応　税区分セット方法変更
*****MOVE    NYU-F10             TO   TO0-F20.
*****2014/01/17 NAV ST 条件判定変更
*****IF      NYU-F10 = SPACE  OR  "0"
     IF      NYU-F10 = SPACE
*****2014/01/17 NAV ED 条件判定変更
             IF   HZN-F26  >=  JBK-F06
                  MOVE   JBK-F15 TO   TO0-F20
             ELSE
                  MOVE   JBK-F14 TO   TO0-F20
             END-IF
     ELSE
             MOVE        NYU-F10 TO   TO0-F20
     END-IF.
*##2013/12/25 NAV ED 消費税増税対応　税区分セット方法変更
*   備考
     MOVE    NYU-F24             TO   TO0-F22.
*   注文_
     MOVE    HAH-F15             TO   TO0-F23.
*   担当者
*## 2008/08/20 内部統制対応 ST ##*
*****MOVE    HAH-F05             TO   TO0-F25.
     MOVE    NYU-F33             TO   TO0-F25.
*   請求区分
     MOVE    NYU-F28             TO   TO0-F26.
*   処理日
     MOVE    SYSYMD2             TO   TO0-F27.
*   伝票区分判定
     EVALUATE     NYU-F01
             WHEN      60
             WHEN      61
             WHEN      50
             WHEN      51
             WHEN      55
             WHEN      56
                  IF   NYU-F111  =    8
                       MOVE      "00999960"
                                 TO   TO0-F11
                  ELSE
                       MOVE      "00999961"
                                 TO   TO0-F11
                  END-IF
             WHEN      52
                  MOVE "0099962" TO   TO0-F11
     END-EVALUATE.
*    直送単価区分
     MOVE    "3"       TO        TO0-F29.
*    単価
     MOVE    ZERO      TO        TO0-F30.
*    金額
     MOVE    NYU-F112  TO        TO0-F31.
*    仕入先ＣＤ
     MOVE    NYU-F08   TO        TO0-F28.
*   支払締日
     MOVE    NYU-F34   TO        TO0-F34.
*    送料区分チェック
     IF      NYU-F111  =         9
*            単価区分
             MOVE      3         TO        TO0-F14
*            単価
             MOVE      ZERO      TO        TO0-F16
*            送料金額
             MOVE      NYU-F112  TO        TO0-F17
     END-IF.
     MOVE      TOK-REC  TO        TOKJ-REC(1:256).
*    送料レコード作成
     WRITE             TOK-REC.
     WRITE             TOKJ-REC.
*    送料区分
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
     MOVE    SPACE               TO   TOKJ1-REC.
     INITIALIZE                       TOKJ1-REC.
*    部門
*****MOVE    WK-BUMON            TO   TO1-F01.
     EVALUATE     NYU-F08(1:3)
         WHEN     291
                  MOVE   "2910"  TO   WK-BUMON-CD1
         WHEN     292
                  MOVE   "2920"  TO   WK-BUMON-CD1
         WHEN     293
                  MOVE   "2930"  TO   WK-BUMON-CD1
         WHEN     294
                  MOVE   "2940"  TO   WK-BUMON-CD1
         WHEN     OTHER
                  MOVE    NYU-F27     TO WK-SOKCD
                  PERFORM HJYOKEN-READ-SEC
                  IF      HJYOKEN-INV-FLG = "INV"
                          GO          TO 220-HENSYU2-EXT
                  ELSE
                          MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
                  END-IF
     END-EVALUATE.
*    部門
     MOVE    WK-BUMON-CD1        TO   TO0-F01.
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
*    発注ファイル読込み
     MOVE    OLD-02              TO   HAH-F02.
     MOVE    ZERO                TO   HAC-INV.
     PERFORM      HACH-READ.
*    発注ファイル未存在
     IF      HAC-INV   NOT =     ZERO
             GO TO               220-HENSYU2-EXT
     END-IF.
*    送付先区分＝１以外
     IF      HAH-F34   NOT =     1
             GO TO               220-HENSYU2-EXT
     END-IF.
*    送先１
     MOVE    HAH-F27             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F07.
*    住所１
     MOVE    HAH-F28             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F08.
*    住所２
     MOVE    HAH-F29             TO   WK-JEF.
     MOVE    WK-XX               TO   JEIC-INDT.
     CALL    "JCVJEIC" USING          JEIC-RTNCD
                                      JEIC-INLT
                                      JEIC-INDT
                                      JEIC-OTLT
                                      JEIC-OTDT
                                      JEIC-DTLT.
     MOVE    JEIC-OTDT           TO   TO1-F09.
*    郵便_
     MOVE    HAH-F26             TO   TO1-F12.
*    担当者
     MOVE    HAH-F05             TO   TO1-F10.
*    処理日
     MOVE    SYSYMD2             TO   TO1-F11.
*    出荷日
     MOVE    HZN-F26             TO   TO1-F13.
     MOVE    TOK1-REC            TO   TOKJ-REC(1:256).
*    計上ファイル追加
     WRITE                       TOK-REC.
     WRITE                       TOKJ-REC.
 220-HENSYU2-EXT.
     EXIT.
************************************************************
*      9000      入庫ファイルの読込処理                    *
************************************************************
 NYUK-READ              SECTION.
     READ    NYKFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             MOVE      HIGH-VALUE     TO   NEW-KEY
             GO        TO        NYUK-READ-EXT
     END-READ.
*    入庫ファイル　計上フラグ＝１以外、計上済フラグ＝０以外
     IF      (NYU-F30  NOT  =    1)   OR
             (NYU-F31  NOT  =    ZERO)
             GO        TO        NYUK-READ
     END-IF.
*****取消が１の時は、対象外 20100219 NAV ST
     IF      (NYU-F97       =   "1")
             GO        TO        NYUK-READ
     END-IF.
************************************ NAV ED
*    キー部セット
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
     READ    HACHEDF
       INVALID      KEY
          INITIALIZE              HAH-REC
          MOVE      9       TO    HAC-INV
       NOT INVALID      KEY
          MOVE      ZERO    TO    HAC-INV
     END-READ.
 HACH-READ-EXT.
     EXIT.
************************************************************
*      発注ファイルの読込
************************************************************
 HACM-READ              SECTION.
     READ    HACMEIF
       INVALID      KEY
          INITIALIZE              HAM-REC
          MOVE      9       TO    HAM-INV
       NOT INVALID      KEY
          MOVE      ZERO    TO    HAM-INV
     END-READ.
 HACM-READ-EXT.
     EXIT.
************************************************************
*      商品コード変換テーブルの読込                      *
************************************************************
 SHO-READ               SECTION.
     MOVE    NYU-F09         TO   SHO-F01.
     MOVE    NYU-F15         TO   SHO-F031.
     MOVE    NYU-F16         TO   SHO-F032.
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
     CLOSE  NYKFILF  HACHEDF  HACMEIF  SHO  TOKU  TOKMS2 TOKUJ.
 300-END-SEC-EXT.
     EXIT.
*--------------------------------------------------------------*
*    条件マスタ読込み
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "20"                 TO   JYO-F01.
     MOVE    WK-SOKCD             TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              DISPLAY NC"＃＃条件Ｆ異常＃＃" UPON  CONS
              DISPLAY NC"＃＃部門ＣＨＫ＃＃" UPON  CONS
              MOVE "INV"          TO   HJYOKEN-INV-FLG
       NOT INVALID KEY
              MOVE SPACE          TO   HJYOKEN-INV-FLG
              MOVE JYO-F05        TO   WK-BUMON-CD
     END-READ.
 HJYOKEN-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
