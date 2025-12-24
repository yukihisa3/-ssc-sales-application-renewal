# SNJ8060V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SNJ8060V.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　オンライン伝票変換エラーＣＳＶ出力*
*    作成日／作成者　　　：　2022/08/10 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　オンライン伝票変換エラーのみを　　*
*    　　　　　　　　　　　　ＣＳＶ出力する。　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNJ8060V.
*                  流用:TNI0210V
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*オンライン伝票変換エラーファイル
     SELECT  ONLERRL1 ASSIGN        TO  ONLERRL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  ERR1-F01 *> バッチ日付　
                                        ERR1-F02 *> バッチ時刻
                                        ERR1-F03 *> バッチ取引先
                                        ERR1-F04 *> 処理日付
                                        ERR1-F05 *> 処理時刻
                                        ERR1-F06 *> 伝票番号
                                        ERR1-F07 *> 店舗ＣＤ
                                        ERR1-F08 *> 納品日
                                        ERR1-F09 *> 行番号
                      FILE   STATUS IS  ERR1-STATUS.
****<< SUB商品名称マスタ　  >>******************************
*    SELECT   SUBMEIL1   ASSIGN  TO   DA-01-VI-SUBMEIL1
*                       ORGANIZATION         IS  INDEXED
*                       ACCESS  MODE         IS  RANDOM
*                       RECORD  KEY          IS  MEI-F011
*                                                MEI-F012
*                       FILE STATUS          IS  MEI-STATUS.
****<< 取引先マスタ　　　  >>******************************
*    SELECT   TOKMS2   ASSIGN  TO   DA-01-VI-TOKMS2
*                       ORGANIZATION         IS  INDEXED
*                       ACCESS  MODE         IS  RANDOM
*                       RECORD  KEY          IS  TOK-F01
*                       FILE STATUS          IS  TOK-STATUS.
****<< 店舗マスタ　　　  >>******************************
*    SELECT   TENMS1   ASSIGN  TO   DA-01-VI-TENMS1
*                       ORGANIZATION         IS  INDEXED
*                       ACCESS  MODE         IS  RANDOM
*                       RECORD  KEY          IS  TEN-F52
*                                                TEN-F011
*                       FILE STATUS          IS  TEN-STATUS.
****<< 伝票変換エラーＣＳＶ >>****************************
     SELECT   ONLERRCV    ASSIGN  TO   DA-01-S-ONLERRCV
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 伝票変換エラーファイル       >>********************
 FD    ONLERRL1.
       COPY     ONLERRL1    OF        XFDLIB
       JOINING  ERR1        AS        PREFIX.
****<< SUB商品名称マスタ　　　>>******************************
*FD    SUBMEIL1.
*      COPY     SUBMEIL1    OF        XFDLIB
*      JOINING  MEI         AS        PREFIX.
****<< 取引先マスタ　　　　　 >>******************************
*FD    TOKMS2.
*      COPY     TOKMS2      OF        XFDLIB
*      JOINING  TOK         AS        PREFIX.
****<< 店舗マスタ　　　　　 >>******************************
*FD    TENMS1.
*      COPY     TENMS1      OF        XFDLIB
*      JOINING  TEN         AS        PREFIX.
****<< オンライン伝票変換エラーＣＳＶ >>************************
 FD    ONLERRCV.
       COPY     ONLERRC4    OF       XFDLIB
       JOINING  CSV         AS       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ 帳票タイトル行       ****
 COPY   ONLERRC1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  ＣＳＶ ヘッダ行　　　       ****
 COPY   ONLERRC2   OF        XFDLIB
        JOINING    CSV2      PREFIX.
****  ＣＳＶ 項目タイトル行　　   ****
 COPY   ONLERRC3   OF        XFDLIB
        JOINING    CSV3      PREFIX.
****  画面制御項目            ****
*01  DSP-CONTROL.
*    02 DSP-PROC             PIC  X(2).
*    02 DSP-GROUP            PIC  X(8).
*    02 DSP-FORMAT           PIC  X(8).
*    02 DSP-STATUS           PIC  X(2).
*    02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ERR1-STATUS          PIC  X(2).
     02 CSV-STATUS           PIC  X(2).
     02 NYY-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 TEN-STATUS           PIC  X(2).
****  カウンタ                ****
 01  ONLERRF-CNT             PIC  9(8)   VALUE  ZERO.
 01  ONLERRCV-CNT            PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
**** エラーメッセージ         ****
*01  ERR-TAB.
*    02  MSG-ERR1            PIC  N(30)  VALUE
*           NC"無効ＰＦキーです。".
*    02  MSG-ERR2            PIC  N(30)  VALUE
*           NC"開始・終了コードの関係に誤りがあります。".
*    02  PMSG01              PIC N(20) VALUE
*           NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SNJ8060V".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*変換１
 01  HENKAN1                 PIC  9(09)V9(02).
 01  HENKAN1R          REDEFINES      HENKAN1.
     03  HENKAN1-1           PIC  9(09).
     03  HENKAN1-2           PIC  9(02).
*
 01  HENKAN1-DATA.
*    03  HENKAN1-DATA0       PIC  X(01).
     03  HENKAN1-DATA1       PIC  X(09).
     03  HENKAN1-DATA2       PIC  X(01).
     03  HENKAN1-DATA3       PIC  X(02).
*変換２
 01  HENKAN2                 PIC  9(01)V9(02).
 01  HENKAN2R          REDEFINES      HENKAN2.
     03  HENKAN2-1           PIC  9(01).
     03  HENKAN2-2           PIC  9(02).
*
 01  HENKAN2-DATA.
     03  HENKAN2-DATA1       PIC  X(01).
     03  HENKAN2-DATA2       PIC  X(01).
     03  HENKAN2-DATA3       PIC  X(02).
*
 01  WK-KANJI.
     03  WK-KANJI1           PIC  N(15)   VALUE  SPACE.
     03  WK-KANJI2           PIC  N(15)   VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE                SECTION.
 01  PARA-IN-BTDATE         PIC   9(08).
 01  PARA-IN-BTTIME         PIC   9(04).
 01  PARA-IN-BTTORI         PIC   9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION
                           USING PARA-IN-BTDATE
                                 PARA-IN-BTTIME
                                 PARA-IN-BTTORI.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ONLERRL1.
     MOVE      "ONLERRL1"    TO   ERR-FL-ID.
     MOVE      ERR1-STATUS   TO   ERR-STCD.
     DISPLAY   MSG-ABEND1        UPON CONS.
     DISPLAY   MSG-ABEND2        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ONLERRCV.
     MOVE   "ONLERRCV"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
*FILEERR-SEC4           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  SUBMEIL1.
*    MOVE   "SUBMEIL1"        TO    ERR-FL-ID.
*    MOVE    MEI-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
*FILEERR-SEC5           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  TOKMS2.
*    MOVE   "TOKMS2 "        TO    ERR-FL-ID.
*    MOVE    TOK-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
*FILEERR-SEC6           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  TENMS1.
*    MOVE   "TENMS1 "        TO    ERR-FL-ID.
*    MOVE    TEN-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SNJ8060V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SNJ8060V-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     OPEN     INPUT     ONLERRL1.
     OPEN     OUTPUT    ONLERRCV.
*
 010-INIT.
*オンライン伝票変換エラーファイル スタート
     PERFORM  ONLERRL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-END
     END-IF.
*オンライン伝票変換エラーファイル 読込
     PERFORM ONLERRL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-END
     END-IF.
*
 021-INIT.
*    帳票タイトルレコード出力
     MOVE     SPACE                   TO       CSV1-REC.
     MOVE     X"28"                   TO       CSV1-CS01.
     MOVE     NC"＜オンライン伝票変換エラーリスト＞"
                                      TO       CSV1-C01.
     MOVE     X"29"                   TO       CSV1-CE01.
     MOVE     ","                     TO       CSV1-CK01
     WRITE    CSV-REC                 FROM     CSV1-REC.
*
 022-INIT.
*    ヘッダレコード出力
     MOVE     SPACE                   TO       CSV2-REC.
     MOVE     X"28"                   TO       CSV2-HS01.
     MOVE     NC"バッチ日付："        TO       CSV2-H01.
     MOVE     X"29"                   TO       CSV2-HE01.
     MOVE     ","                     TO       CSV2-HK01.
     MOVE     ERR1-F01(1:4)           TO       CSV2-H02(1:4).
     MOVE     "/"                     TO       CSV2-H02(5:1).
     MOVE     ERR1-F01(5:2)           TO       CSV2-H02(6:2).
     MOVE     "/"                     TO       CSV2-H02(8:1).
     MOVE     ERR1-F01(7:2)           TO       CSV2-H02(9:2).
     MOVE     ","                     TO       CSV2-HK02.
     MOVE     X"28"                   TO       CSV2-HS03.
     MOVE     NC"バッチ時刻："        TO       CSV2-H03.
     MOVE     X"29"                   TO       CSV2-HE03.
     MOVE     ","                     TO       CSV2-HK03.
     MOVE     ERR1-F02(1:2)           TO       CSV2-H04(1:2).
     MOVE     ":"                     TO       CSV2-H04(3:1).
     MOVE     ERR1-F02(3:2)           TO       CSV2-H04(4:2).
     MOVE     ","                     TO       CSV2-HK04.
     MOVE     X"28"                   TO       CSV2-HS05.
     MOVE     NC"バッチ取引先："      TO       CSV2-H05.
     MOVE     X"29"                   TO       CSV2-HE05.
     MOVE     ","                     TO       CSV2-HK05.
     MOVE     ERR1-F03                TO       CSV2-H06.
     MOVE     ","                     TO       CSV2-HK06.
     WRITE    CSV-REC                 FROM     CSV2-REC.
*
 023-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO       CSV3-REC.
     MOVE     X"28"                   TO       CSV3-KS01
                                               CSV3-KS02
                                               CSV3-KS03
                                               CSV3-KS04
                                               CSV3-KS05
                                               CSV3-KS06
                                               CSV3-KS07
                                               CSV3-KS08
                                               CSV3-KS09
                                               CSV3-KS10
                                               CSV3-KS11
                                               CSV3-KS12
                                               CSV3-KS13
                                               CSV3-KS14
                                               CSV3-KS15
                                               CSV3-KS16
                                               CSV3-KS17
                                               CSV3-KS18
                                               CSV3-KS19
                                               CSV3-KS20
                                               CSV3-KS21
                                               CSV3-KS22
                                               CSV3-KS23
                                               CSV3-KS24
                                               CSV3-KS25
                                               CSV3-KS26
                                               CSV3-KS27
                                               CSV3-KS28.
     MOVE     X"29"                   TO       CSV3-KE01
                                               CSV3-KE02
                                               CSV3-KE03
                                               CSV3-KE04
                                               CSV3-KE05
                                               CSV3-KE06
                                               CSV3-KE07
                                               CSV3-KE08
                                               CSV3-KE09
                                               CSV3-KE10
                                               CSV3-KE11
                                               CSV3-KE12
                                               CSV3-KE13
                                               CSV3-KE14
                                               CSV3-KE15
                                               CSV3-KE16
                                               CSV3-KE17
                                               CSV3-KE18
                                               CSV3-KE19
                                               CSV3-KE20
                                               CSV3-KE21
                                               CSV3-KE22
                                               CSV3-KE23
                                               CSV3-KE24
                                               CSV3-KE25
                                               CSV3-KE26
                                               CSV3-KE27
                                               CSV3-KE28.
     MOVE     ","                     TO       CSV3-KK01
                                               CSV3-KK02
                                               CSV3-KK03
                                               CSV3-KK04
                                               CSV3-KK05
                                               CSV3-KK06
                                               CSV3-KK07
                                               CSV3-KK08
                                               CSV3-KK09
                                               CSV3-KK10
                                               CSV3-KK11
                                               CSV3-KK12
                                               CSV3-KK13
                                               CSV3-KK14
                                               CSV3-KK15
                                               CSV3-KK16
                                               CSV3-KK17
                                               CSV3-KK18
                                               CSV3-KK19
                                               CSV3-KK20
                                               CSV3-KK21
                                               CSV3-KK22
                                               CSV3-KK23
                                               CSV3-KK24
                                               CSV3-KK25
                                               CSV3-KK26
                                               CSV3-KK27
                                               CSV3-KK28.
     MOVE   NC"伝票番号"               TO      CSV3-K01.
     MOVE   NC"店舗ＣＤ"               TO      CSV3-K02.
     MOVE   NC"納品日"                 TO      CSV3-K03.
     MOVE   NC"行番号"                 TO      CSV3-K04.
     MOVE   NC"出荷場所"               TO      CSV3-K05.
     MOVE   NC"発注日"                 TO      CSV3-K06.
     MOVE   NC"相手商品ＣＤ"           TO      CSV3-K07.
     MOVE   NC"サカタ商品ＣＤ"         TO      CSV3-K08.
     MOVE   NC"サカタ品単"             TO      CSV3-K09.
     MOVE   NC"商品名カナ１"           TO      CSV3-K10.
     MOVE   NC"商品名カナ２"           TO      CSV3-K11.
     MOVE   NC"数量"                   TO      CSV3-K12.
     MOVE   NC"原価単価"               TO      CSV3-K13.
     MOVE   NC"マスタ原価単価"         TO      CSV3-K14.
     MOVE   NC"売価単価"               TO      CSV3-K15.
     MOVE   NC"マスタ売価単価"         TO      CSV3-K16.
     MOVE   NC"取引先マスタ未登録"     TO      CSV3-K17.
     MOVE   NC"店舗マスタ未登録"       TO      CSV3-K18.
     MOVE   NC"ＳＵＢ商品変換ＴＢＬ未登録" TO  CSV3-K19.
     MOVE   NC"商品変換ＴＢＬ未登録"   TO      CSV3-K20.
     MOVE   NC"商品名称マスタ未登録"   TO      CSV3-K21.
     MOVE   NC"単価エラー"             TO      CSV3-K22.
     MOVE   NC"売上２重計上"           TO      CSV3-K23.
     MOVE   NC"エラー区分８"           TO      CSV3-K24.
     MOVE   NC"エラー区分９"           TO      CSV3-K25.
     MOVE   NC"エラー区分１０"         TO      CSV3-K26.
     MOVE   NC"処理日付"               TO      CSV3-K27.
     MOVE   NC"処理時刻"               TO      CSV3-K28.
     WRITE  CSV-REC                    FROM    CSV3-REC.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     ADD        1           TO      ONLERRF-CNT.
*
*オンライン伝票変換エラーＣＳＶ出力
 MAIN-01.
*
     PERFORM  ONLERRCV-WRITE1-SEC.
*
 MAIN-02.
     PERFORM  ONLERRL1-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*    オンライン伝票変換エラーファイル スタート
****************************************************************
 ONLERRL1-START-SEC          SECTION.
*
*    MOVE    "ONLERRL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   ERR1-REC.
     INITIALIZE                        ERR1-REC.
     MOVE     PARA-IN-BTDATE      TO   ERR1-F01.
     MOVE     PARA-IN-BTTIME      TO   ERR1-F02.
     MOVE     PARA-IN-BTTORI      TO   ERR1-F03.
*
     START  ONLERRL1  KEY  IS  >=      ERR1-F01
                                       ERR1-F02
                                       ERR1-F03
                                       ERR1-F04
                                       ERR1-F05
                                       ERR1-F06
                                       ERR1-F07
                                       ERR1-F08
                                       ERR1-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 ONLERRL1-START-EXIT.
     EXIT.
*
****************************************************************
*    オンライン伝票変換エラーファイル 読込
****************************************************************
 ONLERRL1-READ-SEC           SECTION.
*
*    MOVE    "ONLERRL1-READ-SEC"  TO   S-NAME.
*
 ONLERRL1-READ-010.
     READ     ONLERRL1       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   ONLERRL1-READ-EXIT
     END-READ.
*バッチ_を判定
     IF       ERR1-F01   NOT =  PARA-IN-BTDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   ONLERRL1-READ-EXIT
     END-IF.
     IF       ERR1-F02   NOT =  PARA-IN-BTTIME
              MOVE     "END"      TO   END-FLG
              GO                  TO   ONLERRL1-READ-EXIT
     END-IF.
     IF       ERR1-F03   NOT =  PARA-IN-BTTORI
              MOVE     "END"      TO   END-FLG
              GO                  TO   ONLERRL1-READ-EXIT
     END-IF.
*エラー区分チェック
     IF     ( ERR1-F23   NOT =  "1" ) AND
            ( ERR1-F24   NOT =  "1" ) AND
            ( ERR1-F25   NOT =  "1" ) AND
            ( ERR1-F26   NOT =  "1" ) AND
            ( ERR1-F27   NOT =  "1" ) AND
            ( ERR1-F28   NOT =  "1" ) AND
            ( ERR1-F29   NOT =  "1" ) AND
            ( ERR1-F30   NOT =  "1" ) AND
            ( ERR1-F31   NOT =  "1" ) AND
            ( ERR1-F32   NOT =  "1" )
              GO                  TO   ONLERRL1-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   ONLERRF-CNT.
*
 ONLERRL1-READ-EXIT.
     EXIT.
*
************************************************************
*      １   伝票変換エラーＣＳＶ出力　L1より　　　         *
************************************************************
 ONLERRCV-WRITE1-SEC       SECTION.
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25 CSV-MK26 CSV-MK27
                                    CSV-MK28.
*    MOVE     X"28"         TO      CSV-MS02 CSV-MS11
*                                   CSV-MS18 CSV-MS19.
*    MOVE     X"29"         TO      CSV-ME02 CSV-ME11
*                                   CSV-ME18 CSV-ME19.
*カウントアップ
     ADD       1            TO      ONLERRCV-CNT.
*
*伝票番号
     MOVE      ERR1-F06      TO      CSV-M01.
*店舗ＣＤ
     MOVE      ERR1-F07      TO      CSV-M02.
*納品日
     MOVE      ERR1-F08(1:4) TO      CSV-M03(1:4).
     MOVE      "/"           TO      CSV-M03(5:1).
     MOVE      ERR1-F08(5:2) TO      CSV-M03(6:2).
     MOVE      "/"           TO      CSV-M03(8:1).
     MOVE      ERR1-F08(7:2) TO      CSV-M03(9:2).
*行番号
     MOVE      ERR1-F09      TO      CSV-M04.
*出荷場所
     MOVE      ERR1-F10      TO      CSV-M05.
*発注日
     MOVE      ERR1-F12(1:4) TO      CSV-M06(1:4).
     MOVE      "/"           TO      CSV-M06(5:1).
     MOVE      ERR1-F12(5:2) TO      CSV-M06(6:2).
     MOVE      "/"           TO      CSV-M06(8:1).
     MOVE      ERR1-F12(7:2) TO      CSV-M06(9:2).
*相手商品ＣＤ
     MOVE      ERR1-F13      TO      CSV-M07.
*サカタ商品ＣＤ
     MOVE      ERR1-F14      TO      CSV-M08.
*サカタ品単ＣＤ
     MOVE      ERR1-F15      TO      CSV-M09.
*商品名カナ１
     MOVE      ERR1-F16      TO      CSV-M10.
*商品名カナ２
     MOVE      ERR1-F17      TO      CSV-M11.
*数量
     IF        ERR1-F18      <       0
               MOVE   "-"    TO      CSV-MA12
     ELSE
               MOVE   "0"    TO      CSV-MA12
     END-IF.
     MOVE      ERR1-F18      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M12.
*原価単価
     IF        ERR1-F19      <       0
               MOVE   "-"    TO      CSV-MA13
     ELSE
               MOVE   "0"    TO      CSV-MA13
     END-IF.
     MOVE      ERR1-F19      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M13.
*マスタ原価単価
     IF        ERR1-F20      <       0
               MOVE   "-"    TO      CSV-MA14
     ELSE
               MOVE   "0"    TO      CSV-MA14
     END-IF.
     MOVE      ERR1-F20      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M14.
*売価単価
     IF        ERR1-F21      <       0
               MOVE   "-"    TO      CSV-MA15
     ELSE
               MOVE   "0"    TO      CSV-MA15
     END-IF.
     MOVE      ERR1-F21      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M15.
*マスタ売価単価
     IF        ERR1-F22      <       0
               MOVE   "-"    TO      CSV-MA16
     ELSE
               MOVE   "0"    TO      CSV-MA16
     END-IF.
     MOVE      ERR1-F22      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M16.
*エラー区分１
     MOVE      ERR1-F23      TO      CSV-M17.
*エラー区分２
     MOVE      ERR1-F24      TO      CSV-M18.
*エラー区分３
     MOVE      ERR1-F25      TO      CSV-M19.
*エラー区分４
     MOVE      ERR1-F26      TO      CSV-M20.
*エラー区分５
     MOVE      ERR1-F27      TO      CSV-M21.
*エラー区分６
     MOVE      ERR1-F28      TO      CSV-M22.
*エラー区分７
     MOVE      ERR1-F29      TO      CSV-M23.
*エラー区分８
     MOVE      ERR1-F30      TO      CSV-M24.
*エラー区分９
     MOVE      ERR1-F31      TO      CSV-M25.
*エラー区分１０
     MOVE      ERR1-F32      TO      CSV-M26.
*処理日付
     MOVE      ERR1-F04(1:4) TO      CSV-M27(1:4).
     MOVE      "/"           TO      CSV-M27(5:1).
     MOVE      ERR1-F04(5:2) TO      CSV-M27(6:2).
     MOVE      "/"           TO      CSV-M27(8:1).
     MOVE      ERR1-F04(7:2) TO      CSV-M27(9:2).
*処理時刻
     MOVE      ERR1-F05(1:2) TO      CSV-M28(1:2).
     MOVE      ":"           TO      CSV-M28(3:1).
     MOVE      ERR1-F05(3:2) TO      CSV-M28(4:2).
     MOVE      ":"           TO      CSV-M28(6:1).
     MOVE      ERR1-F05(5:2) TO      CSV-M28(7:2).
*
*レコード出力
     WRITE     CSV-REC.

 ONLERRCV-WRITE1-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE   ONLERRCV ONLERRL1.
*
     IF      ONLERRF-CNT NOT = ZERO
             DISPLAY  " ONLERRF-READ-CNT   = " ONLERRF-CNT
                                                    UPON CONS
             DISPLAY  " ONLERRCV-WRITE-CNT = " ONLERRCV-CNT
                                                    UPON CONS
     ELSE
             DISPLAY  " ONLERRF-READ-CNT   = " ONLERRF-CNT
                                                    UPON CONS
             DISPLAY  " ONLERRCV-WRITE-CNT = " ONLERRCV-CNT
                                                    UPON CONS
             MOVE    4010      TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
