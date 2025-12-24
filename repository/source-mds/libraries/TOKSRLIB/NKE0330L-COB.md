# NKE0330L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0330L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　返品検品　　　　　　　            *
*    モジュール名　　　　：　返品検品結果確認リスト発行　　　　*
*    処理概要　　　　　　：　返品各種ファイルより、　　　　　　*
*                            条件に合致するリストを出力する。　*
*    流用　　　　　　　　：　NKE0260L                          *
*    作成日／作成者　　　：　2019/01/24 INOUE                  *
*    更新日／更新者　　　：　2019/01/28 INOUE                  *
*    更新概要　　　　　　：　変換担当者マスタ追加　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0330L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/24.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ホスト返品管理ファイル
     SELECT  HENKANL2   ASSIGN    TO        DA-01-VI-HENKANL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAN-F01
                                            KAN-F98
                                            KAN-F99
                                            KAN-F02
                                            KAN-F03
                                            KAN-F04
                                            KAN-F05
                        FILE  STATUS   IS   KAN-STATUS.
*ホスト返品管理明細ファイル
     SELECT  HENKAML2   ASSIGN    TO        DA-01-VI-HENKAML2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAM-F01
                                            KAM-F98
                                            KAM-F99
                                            KAM-F02
                                            KAM-F03
                                            KAM-F04
                                            KAM-F05
                                            KAM-F06
                        FILE  STATUS   IS   KAM-STATUS.
*ホスト返品実績ファイル
     SELECT  HENKJSL2   ASSIGN    TO        DA-01-VI-HENKJSL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KJS-F01
                                            KJS-F98
                                            KJS-F99
                                            KJS-F02
                                            KJS-F03
                                            KJS-F04
                                            KJS-F05
                                            KJS-F06
                                            KJS-F11
                                            KJS-F12
                                            KJS-F13
                        FILE  STATUS   IS   KJS-STATUS.
*倉庫マスタ
     SELECT    ZSOKMS1  ASSIGN    TO       DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      SOK-F01
                        FILE      STATUS   SOK-STATUS.
*取引先マスタ
     SELECT    TOKMS2   ASSIGN    TO       DA-01-VI-TOKMS2
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TOK-F01
                        FILE      STATUS   TOK-STATUS.
*店舗マスタ
     SELECT    TENMS1   ASSIGN    TO       DA-01-VI-TENMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TEN-F52  TEN-F011
                        FILE      STATUS   TEN-STATUS.
*仕入先マスタ
*    SELECT    ZSHIMS1  ASSIGN    TO       DA-01-VI-ZSHIMS1
*                       ORGANIZATION       INDEXED
*                       ACCESS    MODE     RANDOM
*                       RECORD    KEY      SHI-F01
*                       FILE      STATUS   SHI-STATUS.
*条件ファイル
*    SELECT    JYOKEN1  ASSIGN    TO       DA-01-VI-JYOKEN1
*                       ORGANIZATION       INDEXED
*                       ACCESS    MODE     RANDOM
*                       RECORD    KEY      JYO-F01  JYO-F02
*                       FILE      STATUS   JYO-STATUS.
*商品名称マスタ
     SELECT    MEIMS1   ASSIGN    TO       DA-01-VI-MEIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                        FILE      STATUS   MEI-STATUS.
*担当者マスタ
     SELECT    TANMS1   ASSIGN    TO       DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TAN-F01  TAN-F02
                        FILE      STATUS   TAN-STATUS.
*2019.01.28↓
*変換担当者マスタ
     SELECT    TANHENL1 ASSIGN    TO       DA-01-VI-TANHENL1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      HEN-F01
                        FILE      STATUS   HEN-STATUS.
*2019.01.28↑
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*ホスト返品管理ファイル
 FD  HENKANL2            LABEL RECORD   IS   STANDARD.
     COPY     HENKANL2   OF        XFDLIB
     JOINING  KAN       PREFIX.
*ホスト返品管理明細ファイル
 FD  HENKAML2            LABEL RECORD   IS   STANDARD.
     COPY     HENKAML2   OF        XFDLIB
     JOINING  KAM       PREFIX.
*ホスト返品実績ファイル
 FD  HENKJSL2            LABEL RECORD   IS   STANDARD.
     COPY     HENKJSL2   OF        XFDLIB
     JOINING  KJS       PREFIX.
*倉庫マスタ
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
     JOINING  SOK       AS        PREFIX.
*取引先マスタ
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
     JOINING  TOK       AS        PREFIX.
*店舗マスタ
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
     JOINING  TEN       AS        PREFIX.
*仕入先マスタ
*FD  ZSHIMS1            LABEL RECORD   IS   STANDARD.
*    COPY     ZSHIMS1   OF        XFDLIB
*    JOINING  SHI       AS        PREFIX.
*条件ファイル
*FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
*    COPY     JYOKEN1   OF        XFDLIB
*    JOINING  JYO       AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*2019.01.28↓
*変換担当者マスタ
 FD  TANHENL1           LABEL RECORD   IS   STANDARD.
     COPY     TANHENL1  OF        XFDLIB
     JOINING  HEN       AS        PREFIX.
*2019.01.28↑
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  HENKANL2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HENKAML2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HENKJSL2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  PRINT-OUT-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEN-CNT                 PIC  9(01)     VALUE  ZERO.
 01  MEI-CNT                 PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  1.
 01  HENKAML2-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HENKJSL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  ZSOKMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*2019.01.28↓
 01  TANHENL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*2019.01.28↑
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-SUURYO               PIC S9(09)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAN-STATUS        PIC  X(02).
     03  KAM-STATUS        PIC  X(02).
     03  KJS-STATUS        PIC  X(02).
     03  SOK-STATUS        PIC  X(02).
     03  SHI-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
*2019.01.28↓
     03  HEN-STATUS        PIC  X(02).
*2019.01.28↑
     03  PRT-STATUS        PIC  X(02).
*BRK項目　エリア
* ホスト返品管理ファイル
*   倉庫ＣＤ(ヘッダ部）
     03  BRK-KAN-F01       PIC  X(02)    VALUE ZERO.
     03  BRK-KAN-F01-FLG   PIC  X(03)    VALUE SPACE.
*   取込日付(ヘッダ部）
     03  BRK-KAN-F98       PIC  9(08)    VALUE ZERO.
     03  BRK-KAN-F98-FLG   PIC  X(03)    VALUE SPACE.
*   取込時刻(ヘッダ部）
     03  BRK-KAN-F99       PIC  9(06)    VALUE ZERO.
     03  BRK-KAN-F99-FLG   PIC  X(03)    VALUE SPACE.
*   取引先ＣＤ(ヘッダ部）
     03  BRK-KAN-F03       PIC  9(08)    VALUE ZERO.
     03  BRK-KAN-F03-FLG   PIC  X(03)    VALUE SPACE.
*   店舗ＣＤ(伝票番号部）
     03  BRK-KAN-F04       PIC  9(05)    VALUE ZERO.
     03  BRK-KAN-F04-FLG   PIC  X(03)    VALUE SPACE.
*   返品日(伝票番号部）
     03  BRK-KAN-F05       PIC  9(08)    VALUE ZERO.
     03  BRK-KAN-F05-FLG   PIC  X(03)    VALUE SPACE.
*   返品検品番号(商品明細部)
     03  BRK-KJS-F06       PIC  9(10)    VALUE ZERO.
     03  BRK-KJS-F06-FLG   PIC  X(03)    VALUE SPACE.
*   検品開始時刻(商品明細部)
     03  BRK-KJS-F11       PIC  9(10)    VALUE ZERO.
     03  BRK-KJS-F11-FLG   PIC  X(03)    VALUE SPACE.
*   検品終了時刻(商品明細部)
     03  BRK-KJS-F12       PIC  9(10)    VALUE ZERO.
     03  BRK-KJS-F12-FLG   PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0330L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0330L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0330L".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD0.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "NKE0330L".
         05  FILLER          PIC  X(33)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  FILLER          PIC  N(11)     VALUE
         NC"＜倉庫返品検品リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(28)     VALUE  SPACE.
         05  HD0-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD0-MM          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD0-DD          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD0-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD00.
     03  FILLER              PIC  X(116)    VALUE  SPACE.
*    03  FILLER         CHARACTER  TYPE YA-22.
*        05  HD00-CKUBUN     PIC  N(07).
*    03  FILLER              PIC  X(38)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-MS         PIC  9(02).
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"倉庫：".
         05  HD000-SOKCD     PIC  X(02).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD000-SOKNM     PIC  N(18).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"取込日付：".
         05  HD000-TDATE     PIC  X(10).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"取込時刻：".
         05  HD000-TTIME     PIC  X(08).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"担当者：".
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)   VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD0000.
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"取引先：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD0000-TOKCD    PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD0000-TOKNM    PIC  N(15).
*
 01  SEN.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"店舗情報".
         05  FILLER          PIC  X(11)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"返品日".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"伝票番号".
*
 01  HD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(03)  VALUE SPACE.
         05  FILLER          PIC  N(06)  VALUE NC"返品検品番号".
         05  FILLER          PIC  X(03)  VALUE SPACE.
         05  FILLER          PIC  N(04)  VALUE NC"検品開始".
         05  FILLER          PIC  X(02)  VALUE SPACE.
         05  FILLER          PIC  N(04)  VALUE NC"検品終了".
         05  FILLER          PIC  X(03)  VALUE SPACE.
         05  FILLER          PIC  N(05)  VALUE NC"ＪＡＮＣＤ".
         05  FILLER          PIC  X(04)  VALUE SPACE.
         05  FILLER          PIC  N(03)  VALUE NC"商品名".
         05  FILLER          PIC  X(20)  VALUE SPACE.
         05  FILLER          PIC  N(05)  VALUE NC"返品検品数".
         05  FILLER          PIC  X(01)  VALUE SPACE.
         05  FILLER          PIC  N(05)  VALUE NC"返品担当者".
         05  FILLER          PIC  X(20)  VALUE SPACE.
         05  FILLER          PIC  X(03)  VALUE "HHT".
*---------------------------------------------------------------
 01  MS011.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS011-TENCD     PIC  X(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS011-TENNM     PIC  N(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS011-HDATE     PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04)     VALUE  NC"＜伝票＞".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS011-DENX.
             07  MS011-DEN                      OCCURS 10.
                 09  MS011-DENNO PIC  X(09).
                 09  FILLER      PIC  X(01)     VALUE  SPACE.
*
 01  MS021.
     03  FILLER              CHARACTER TYPE YA.
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS021-KENNO     PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS021-TIME-F    PIC  X(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS021-TIME-T    PIC  X(08).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS021-JANCD     PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS021-SYONAME   PIC  X(25).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS021-KENSU     PIC  --,---,--9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS021-TANCD     PIC  X(07).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS021-TANNM     PIC  N(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS021-HHT       PIC  9(04).
*---------------------------------------------------------------
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　対象データは０件です　！！　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*---------------------------------------------------------------
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*---------------------------------------------------------------
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
 01  PARA-IN-SOKCD         PIC   X(02).
 01  PARA-IN-DATEF         PIC   9(08).
 01  PARA-IN-DATET         PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION
                                 USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-SOKCD
                                       PARA-IN-DATEF
                                       PARA-IN-DATET.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HENKANL2.
     MOVE      "HENKANL2"   TO   AB-FILE.
     MOVE      KAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HENKAML2.
     MOVE      "HENKAML2"   TO   AB-FILE.
     MOVE      KAM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HENKJSL2.
     MOVE      "HENKJSL2"   TO   AB-FILE.
     MOVE      KJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1 "   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*2019.01.28↓
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANHENL1.
     MOVE      "TANHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*2019.01.28↑
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     HENKANL2
                        HENKAML2
                        HENKJSL2
                        ZSOKMS1
                        TOKMS2
                        TENMS1
                        MEIMS1
                        TANMS1
*2019.01.28↓
                        TANHENL1.
*2019.01.28↑
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME          FROM   TIME.
*ホスト返品管理ファイルスタート
     PERFORM  HENKANL2-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*ホスト返品管理ファイル読込
     PERFORM HENKANL2-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし．＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*ブレイクＫＥＹセット (ホスト返品管理ファイル)
*   倉庫ＣＤ(ヘッダ部）
     MOVE    KAN-F01            TO    BRK-KAN-F01.
*   取込日付(ヘッダ部）
     MOVE    KAN-F98            TO    BRK-KAN-F98.
*   取込時刻(ヘッダ部）
     MOVE    KAN-F99            TO    BRK-KAN-F99.
*   取引先ＣＤ(ヘッダ部）
     MOVE    KAN-F03            TO    BRK-KAN-F03.
*   店舗ＣＤ(明細部）
     MOVE    KAN-F04            TO    BRK-KAN-F04.
*   返品日(明細部）
     MOVE    KAN-F05            TO    BRK-KAN-F05.
*
     MOVE    "STR"              TO    BRK-KAN-F04-FLG.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホスト返品管理ファイルスタート
****************************************************************
 HENKANL2-START-SEC          SECTION.
*
     MOVE    "HENKANL2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   KAN-REC.
     INITIALIZE                        KAN-REC.
*
     MOVE     PARA-IN-SOKCD       TO   KAN-F01.
     MOVE     PARA-IN-DATEF       TO   KAN-F98.
     MOVE     ZERO                TO   KAN-F99.
     MOVE     ZERO                TO   KAN-F02.
     MOVE     ZERO                TO   KAN-F03.
     MOVE     ZERO                TO   KAN-F04.
     MOVE     ZERO                TO   KAN-F05.
*
     START    HENKANL2  KEY  IS  >=    KAN-F01 KAN-F98 KAN-F99
                                       KAN-F02 KAN-F03 KAN-F04
                                       KAN-F05
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 HENKANL2-START-EXIT.
     EXIT.
*
****************************************************************
*    ホスト返品管理ファイル読込
****************************************************************
 HENKANL2-READ-SEC           SECTION.
*
     MOVE    "HENKANL2-READ-SEC"  TO   S-NAME.
*
 HENKANL2-READ-01.
     READ     HENKANL2  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   HENKANL2-READ-EXIT
     END-READ.
*条件範囲内判定
*倉庫ＣＤ　対象チェック
     IF       PARA-IN-SOKCD  NOT =  SPACE
         IF   KAN-F01 NOT =  PARA-IN-SOKCD
              MOVE  "END"    TO   END-FLG
              GO             TO   HENKANL2-READ-EXIT
         END-IF
     END-IF.
*取込日付　対象範囲チェック
     IF       PARA-IN-DATEF  NOT = ZERO
        IF    KAN-F98 < PARA-IN-DATEF
              GO             TO   HENKANL2-READ-01
        END-IF
     END-IF.
     IF       PARA-IN-DATET  NOT = ZERO
        IF    KAN-F98 > PARA-IN-DATET
              MOVE  "END"    TO   END-FLG
              GO             TO   HENKANL2-READ-EXIT
        END-IF
     END-IF.
*
*件数カウント
     ADD      1                   TO   HENKANL2-READ-CNT.
*
 HENKANL2-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*明細印刷処理１（伝票番号一覧行）
     PERFORM  MEISAI-WT1-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
*明細印刷処理２（商品一覧行）
     PERFORM  MEISAI-WT2-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-030.
*ホスト返品管理ファイル読込
     PERFORM  HENKANL2-READ-SEC.
*
*ホスト返品管理ファイル ＫＥＹブレイク判定
 MAIN-040.
     IF       KAN-F98  NOT = BRK-KAN-F98
              MOVE     "BRK"     TO   BRK-KAN-F98-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F98-FLG
     END-IF.
*
 MAIN-050.
     IF       KAN-F99  NOT = BRK-KAN-F99
              MOVE     "BRK"     TO   BRK-KAN-F99-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F99-FLG
     END-IF.
*
 MAIN-060.
     IF       KAN-F01  NOT = BRK-KAN-F01
              MOVE     "BRK"     TO   BRK-KAN-F01-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F01-FLG
     END-IF.
*
 MAIN-070.
     IF       KAN-F03  NOT = BRK-KAN-F03
              MOVE     "BRK"     TO   BRK-KAN-F03-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F03-FLG
     END-IF.
*
 MAIN-080.
     IF       KAN-F04  NOT = BRK-KAN-F04
              MOVE     "BRK"     TO   BRK-KAN-F04-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F04-FLG
     END-IF.
*
 MAIN-090.
     IF       KAN-F05  NOT = BRK-KAN-F05
              MOVE     "BRK"     TO   BRK-KAN-F05-FLG
     ELSE
              MOVE     "   "     TO   BRK-KAN-F05-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    ヘッダ印字
****************************************************************
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD0-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD0-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD0-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD0-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    帳票区分セット
*    EVALUATE  PARA-IN-CKUBUN
*      WHEN "1"   MOVE  NC"（　全　件　）" TO   HD00-CKUBUN
*      WHEN " "   MOVE  NC"（エラーのみ）" TO   HD00-CKUBUN
*      WHEN OTHER MOVE  NC"（？？？？？）" TO   HD00-CKUBUN
*    END-EVALUATE.
*    倉庫名取得
     MOVE     KAN-F01             TO   SOK-F01 HD000-SOKCD.
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = SPACE
              MOVE  SOK-F02       TO   HD000-SOKNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-SOKNM
     END-IF.
*    取込日付セット
     MOVE     KAN-F98(1:4)        TO   HD000-TDATE(1:4).
     MOVE     "/"                 TO   HD000-TDATE(5:1).
     MOVE     KAN-F98(5:2)        TO   HD000-TDATE(6:2).
     MOVE     "/"                 TO   HD000-TDATE(8:1).
     MOVE     KAN-F98(7:2)        TO   HD000-TDATE(9:2).
*    取込時刻セット
     MOVE     KAN-F99(1:2)        TO   HD000-TTIME(1:2).
     MOVE     ":"                 TO   HD000-TTIME(3:1).
     MOVE     KAN-F99(3:2)        TO   HD000-TTIME(4:2).
     MOVE     ":"                 TO   HD000-TTIME(6:1).
     MOVE     KAN-F99(5:2)        TO   HD000-TTIME(7:2).
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = SPACE
              MOVE  TAN-F03       TO   HD000-TRTANNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-TRTANNM
     END-IF.
*    取引先名取得
     MOVE     KAN-F03             TO   HD0000-TOKCD
                                       TOK-F01
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = SPACE
              MOVE  TOK-F02       TO   HD0000-TOKNM
     ELSE
              MOVE  ALL NC"？"    TO   HD0000-TOKNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD0     AFTER  2.
     WRITE    PRT-REC       FROM  HD00    AFTER  1.
     WRITE    PRT-REC       FROM  HD000   AFTER  2.
     WRITE    PRT-REC       FROM  HD0000  AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     WRITE    PRT-REC       FROM  HD01    AFTER  1.
     WRITE    PRT-REC       FROM  HD02    AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     MOVE     SPACE               TO      PRT-REC.
*    WRITE    PRT-REC                     AFTER  1.
*行カウントアップ
     MOVE     11                  TO      LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*    明細印字１（伝票番号一覧行）
****************************************************************
 MEISAI-WT1-SEC          SECTION.
     MOVE    "MEISAI-WT1-SEC"      TO   S-NAME.
*
 MEISAI-WT1-01.
*   ブレイク改頁判定
     IF     ( BRK-KAN-F01-FLG     =   "BRK" ) OR
            ( BRK-KAN-F98-FLG     =   "BRK" ) OR
            ( BRK-KAN-F99-FLG     =   "BRK" ) OR
            ( BRK-KAN-F03-FLG     =   "BRK" )
              PERFORM  HEAD-WT-SEC
              MOVE     KAN-F01    TO   BRK-KAN-F01
              MOVE     KAN-F98    TO   BRK-KAN-F98
              MOVE     KAN-F99    TO   BRK-KAN-F99
              MOVE     KAN-F03    TO   BRK-KAN-F03
              MOVE     KAN-F04    TO   BRK-KAN-F04
              MOVE     KAN-F05    TO   BRK-KAN-F05
              MOVE     "   "      TO   BRK-KAN-F01-FLG
              MOVE     "   "      TO   BRK-KAN-F98-FLG
              MOVE     "   "      TO   BRK-KAN-F99-FLG
              MOVE     "   "      TO   BRK-KAN-F03-FLG
              MOVE     "STR"      TO   BRK-KAN-F04-FLG
              MOVE     "   "      TO   BRK-KAN-F05-FLG
     END-IF.
 MEISAI-WT1-02.
*   改頁判定
     IF       LINE-CNT >  50
              PERFORM  HEAD-WT-SEC
              MOVE    "STR"                TO   BRK-KAN-F04-FLG
     END-IF.
*
 MEISAI-WT1-03.
*  ページ内　店舗ブレイク時の明細１行目
     IF     ( BRK-KAN-F04-FLG     =   "BRK" )
        OR  ( BRK-KAN-F05-FLG     =   "BRK" )
*             線行印刷
              WRITE    PRT-REC     FROM  SEN    AFTER  1
*             行カウント
              ADD      1                   TO   LINE-CNT
     END-IF.
*
 MEISAI-WT1-04.
*明細１行目(店舗・返品日：返品管理ファイル）
*明細１行目(伝票番号　　：返品管理明細ファイル)
*  １ページ目 OR 改ページ後先頭の明細１行目
*  ページ内 　店舗ブレイク時の明細１行目
     IF     ( BRK-KAN-F04-FLG     =   "STR" ) OR
            ( BRK-KAN-F04-FLG     =   "BRK" )
        OR  ( BRK-KAN-F05-FLG     =   "BRK" )
*             ブレイク値入替
              MOVE     KAN-F04             TO   BRK-KAN-F04
              MOVE     KAN-F05             TO   BRK-KAN-F05
*             店舗
              MOVE     KAN-F03             TO   TEN-F52
              MOVE     KAN-F04             TO   TEN-F011
                                                MS011-TENCD
              PERFORM  TENMS1-READ-SEC
              IF       TENMS1-INV-FLG = SPACE
* 正式でなく略？       MOVE  TEN-F02       TO   MS011-TENNM
                       MOVE  TEN-F03       TO   MS011-TENNM
              ELSE
                       MOVE  ALL NC"？"    TO   MS011-TENNM
              END-IF
*             返品日
              MOVE     KAN-F05(1:4)        TO   MS011-HDATE(1:4)
              MOVE     "/"                 TO   MS011-HDATE(5:1)
              MOVE     KAN-F05(5:2)        TO   MS011-HDATE(6:2)
              MOVE     "/"                 TO   MS011-HDATE(8:1)
              MOVE     KAN-F05(7:2)        TO   MS011-HDATE(9:2)
*
*             ホスト返品管理明細ファイルＳＴＡＲＴ
              PERFORM HENKAML2-START-SEC
              MOVE     1                   TO   DEN-CNT
     END-IF.
*
     PERFORM  UNTIL ( HENKAML2-INV-FLG = "BRK" ) OR
                    ( HENKAML2-INV-FLG = "END" )
*             ホスト返品管理明細ファイル読込
              PERFORM HENKAML2-READ-SEC
*              →ＨＩＴ
                 IF    ( HENKAML2-INV-FLG = "   " ) AND
                       ( IDX             <= 10    )
*                        伝票番号
                         MOVE  KAM-F06(2:9)
                                        TO  MS011-DENNO(IDX)
                         ADD   1        TO  IDX
                 END-IF
                 IF    ( HENKAML2-INV-FLG = "   " ) AND
                       ( IDX              > 10    )
                         WRITE PRT-REC  FROM  MS011 AFTER 1
*                        行カウント
                         ADD   1        TO  LINE-CNT
                         MOVE  1        TO  IDX
                         MOVE  SPACE    TO  MS011-DENX
                         MOVE  SPACE    TO  MS011-TENCD
                                            MS011-TENNM
                                            MS011-HDATE
                 END-IF
                 IF    ( HENKAML2-INV-FLG = "BRK" ) OR
                       ( HENKAML2-INV-FLG = "END" )
                    IF   IDX   >  1
                         WRITE PRT-REC  FROM  MS011 AFTER 1
*                        行カウント
                         ADD   1        TO  LINE-CNT
                         MOVE  1        TO  IDX
                         MOVE  "   "    TO  BRK-KAN-F04-FLG
                         MOVE  SPACE    TO  MS011-DENX
                    END-IF
                 END-IF
     END-PERFORM.
*
 MEISAI-WT1-05.
*
*  ページ内　店舗ブレイク時の明細１行目
     IF       BRK-KAN-F04-FLG     =   "BRK"
*             ブレイクリセット
              MOVE     KAN-F04             TO   BRK-KAN-F04
              MOVE     "   "               TO   BRK-KAN-F04-FLG
              MOVE     "   "               TO   HENKAML2-INV-FLG
     END-IF.
*
 MEISAI-WT1-EXIT.
     EXIT.
****************************************************************
*    明細印字２（商品明細行）
****************************************************************
 MEISAI-WT2-SEC          SECTION.
     MOVE    "MEISAI-WT2-SEC"      TO   S-NAME.
*
 MEISAI-WT2-00.
*ホスト返品実績ファイルＳＴＡＲＴ
     PERFORM  HENKJSL2-START-SEC.
     IF       HENKJSL2-INV-FLG     =    "END"
              GO                   TO   MEISAI-WT2-EXIT
     END-IF.
 MEISAI-WT2-01.
*ホスト返品実績ファイル初期ＲＥＡＤ　
     PERFORM  HENKJSL2-READ-SEC.
     IF       HENKJSL2-INV-FLG     =    "END"
              GO                   TO   MEISAI-WT2-EXIT
     END-IF.
     MOVE     KJS-F11              TO   BRK-KJS-F11.
     MOVE     KJS-F12              TO   BRK-KJS-F12.
*
 MEISAI-WT2-03.
*明細(返品実績ファイル)
     MOVE     1                   TO   MEI-CNT.
     PERFORM  UNTIL ( HENKJSL2-INV-FLG = "BRK" ) OR
                    ( HENKJSL2-INV-FLG = "END" )
*          改頁判定
           IF       LINE-CNT >  50
                    PERFORM  HEAD-WT-SEC
           END-IF
*
*        　ＨＩＴ
           IF   HENKJSL2-INV-FLG = "   "
*             トータルインジケート制御
*               返品検品番号イコール
                IF       KJS-F06         =    BRK-KJS-F06
*                  返品開始・終了イコール
                   IF  ( KJS-F11         =    BRK-KJS-F11 ) AND
                       ( KJS-F12         =    BRK-KJS-F12 )
                         MOVE   2        TO   MEI-CNT
*                  返品開始OR終了ブレイク
                   ELSE
                         MOVE   3        TO   MEI-CNT
                         MOVE   KJS-F11  TO   BRK-KJS-F11
                         MOVE   KJS-F12  TO   BRK-KJS-F12
                   END-IF
*              返品検品番号ブレイク
               ELSE
                   MOVE  1               TO   MEI-CNT
                   MOVE  KJS-F06         TO   BRK-KJS-F06
               END-IF
*
               IF  MEI-CNT            =  1
*                  返品検品番号
                   MOVE  KJS-F06         TO   MS021-KENNO
               ELSE
                   MOVE  SPACE           TO   MS021-KENNO
               END-IF
*
               IF  MEI-CNT            =  2
                   MOVE  SPACE           TO   MS021-TIME-F
                                              MS021-TIME-T
               ELSE
*                  検品開始時刻
                   MOVE  KJS-F11(1:2) TO MS021-TIME-F(1:2)
                   MOVE  ":"          TO MS021-TIME-F(3:1)
                   MOVE  KJS-F11(3:2) TO MS021-TIME-F(4:2)
                   MOVE  ":"          TO MS021-TIME-F(6:1)
                   MOVE  KJS-F11(5:2) TO MS021-TIME-F(7:2)
*                  検品終了時刻
                   MOVE  KJS-F12(1:2) TO MS021-TIME-T(1:2)
                   MOVE  ":"          TO MS021-TIME-T(3:1)
                   MOVE  KJS-F12(3:2) TO MS021-TIME-T(4:2)
                   MOVE  ":"          TO MS021-TIME-T(6:1)
                   MOVE  KJS-F12(5:2) TO MS021-TIME-T(7:2)
               END-IF
*
*              ＪＡＮＣＤ
               MOVE  KJS-F13             TO MS021-JANCD
*              商品名
               MOVE  KJS-F15             TO MEI-F011
               MOVE  KJS-F16             TO MEI-F0121
               MOVE  KJS-F17             TO MEI-F0122
               MOVE  KJS-F18             TO MEI-F0123
               PERFORM  MEIMS1-READ-SEC
               IF       MEIMS1-INV-FLG = SPACE
                        MOVE  MEI-F031   TO MS021-SYONAME
                        MOVE  MEI-F032   TO MS021-SYONAME(16:10)
               ELSE
                        MOVE  ALL "?"    TO MS021-SYONAME
               END-IF
*              返品検品数
               MOVE  KJS-F14             TO MS021-KENSU
*              返品担当者
*2019.01.28↓
*-----         MOVE  PARA-IN-BUMON       TO MS021-TANCD(1:4)
*-----                                      TAN-F01
*-----         MOVE  "-"                 TO MS021-TANCD(5:1)
*-----         MOVE  KJS-F10(2:2)        TO MS021-TANCD(6:2)
*-----                                      TAN-F02
*-----         PERFORM  TANMS1-READ-SEC
*-----         IF       TANMS1-INV-FLG = SPACE
*-----                  MOVE  TAN-F03    TO MS021-TANNM
*-----         ELSE
*-----                  MOVE  ALL NC"？" TO MS021-TANNM
*-----         END-IF
               MOVE     KJS-F10          TO HEN-F01
               PERFORM  TANHENL1-READ-SEC
               IF       TANHENL1-INV-FLG = SPACE
                        MOVE  HEN-F02    TO TAN-F01
                                            MS021-TANCD(1:4)
                        MOVE  "-"        TO MS021-TANCD(5:1)
                        MOVE  HEN-F03    TO TAN-F02
                                            MS021-TANCD(6:2)
                        PERFORM  TANMS1-READ-SEC
                        IF       TANMS1-INV-FLG = SPACE
                                 MOVE  TAN-F03    TO MS021-TANNM
                        ELSE
                              MOVE  "????" TO MS021-TANCD(1:4)
                              MOVE  "-"    TO MS021-TANCD(5:1)
                              MOVE  KJS-F10(2:2)
                                           TO MS021-TANCD(6:2)
                              MOVE  ALL NC"？" TO MS021-TANNM
                        END-IF
               ELSE
                        MOVE  "????"       TO MS021-TANCD(1:4)
                        MOVE  "-"          TO MS021-TANCD(5:1)
                        MOVE  KJS-F10(2:2) TO MS021-TANCD(6:2)
                        MOVE  ALL NC"？" TO MS021-TANNM
               END-IF
*2019.01.28↑
*              HHT端末番号
               MOVE  KJS-F09             TO MS021-HHT
*
               WRITE PRT-REC  FROM  MS021 AFTER 1
*              行カウント
               ADD   1                   TO LINE-CNT
               MOVE  2                   TO MEI-CNT
*
*              ホスト返品実績ファイル次読込
               PERFORM HENKJSL2-READ-SEC
*
           END-IF
*
     END-PERFORM.
*
 MEISAI-WT2-EXIT.
     EXIT.
****************************************************************
*    ホスト返品管理明細ファイルスタート
****************************************************************
 HENKAML2-START-SEC          SECTION.
*
     MOVE    "HENKAML2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   KAM-REC.
     INITIALIZE                        KAM-REC.
*
     MOVE     KAN-F01             TO   KAM-F01.
     MOVE     KAN-F98             TO   KAM-F98.
     MOVE     KAN-F99             TO   KAM-F99.
     MOVE     KAN-F02             TO   KAM-F02.
     MOVE     KAN-F03             TO   KAM-F03.
     MOVE     KAN-F04             TO   KAM-F04.
     MOVE     KAN-F05             TO   KAM-F05.
     MOVE     ZERO                TO   KAM-F06.
*
     START    HENKAML2  KEY  IS  >=    KAM-F01 KAM-F98 KAM-F99
                                       KAM-F02 KAM-F03 KAM-F04
                                       KAM-F05 KAM-F06
            INVALID
                 MOVE    "END"         TO   HENKAML2-INV-FLG
            NOT INVALID
                 MOVE    "   "         TO   HENKAML2-INV-FLG
     END-START.
*
 HENKAML2-START-EXIT.
     EXIT.
*
****************************************************************
*    ホスト返品管理明細ファイル読込
****************************************************************
 HENKAML2-READ-SEC           SECTION.
*
     MOVE    "HENKAML2-READ-SEC"  TO   S-NAME.
*
 HENKAML2-READ-01.
     READ     HENKAML2  AT  END
              MOVE     "END"      TO   HENKAML2-INV-FLG
              GO                  TO   HENKAML2-READ-EXIT
     END-READ.
*条件範囲内判定
*対象範囲チェック　倉庫ＣＤ
*  倉庫・取込日・時刻・返品Ｇ・取引先・店舗・返品日
     IF     ( KAM-F01 NOT =  KAN-F01 ) OR
            ( KAM-F98 NOT =  KAN-F98 ) OR
            ( KAM-F99 NOT =  KAN-F99 ) OR
            ( KAM-F02 NOT =  KAN-F02 ) OR
            ( KAM-F03 NOT =  KAN-F03 ) OR
            ( KAM-F04 NOT =  KAN-F04 ) OR
            ( KAM-F05 NOT =  KAN-F05 )
              MOVE  "BRK"    TO   HENKAML2-INV-FLG
              GO             TO   HENKAML2-READ-EXIT
     ELSE
              MOVE  "   "    TO   HENKAML2-INV-FLG
     END-IF.
*
*件数カウント
     ADD      1                   TO   HENKAML2-READ-CNT.
*
 HENKAML2-READ-EXIT.
     EXIT.
*
****************************************************************
*    ホスト返品実績ファイルスタート
****************************************************************
 HENKJSL2-START-SEC          SECTION.
*
     MOVE    "HENKJSL2-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   KJS-REC.
     INITIALIZE                        KJS-REC.
*
     MOVE     KAN-F01             TO   KJS-F01.
     MOVE     KAN-F98             TO   KJS-F98.
     MOVE     KAN-F99             TO   KJS-F99.
     MOVE     KAN-F02             TO   KJS-F02.
     MOVE     KAN-F03             TO   KJS-F03.
     MOVE     KAN-F04             TO   KJS-F04.
     MOVE     KAN-F05             TO   KJS-F05.
     MOVE     ZERO                TO   KJS-F06.
     MOVE     ZERO                TO   KJS-F11.
     MOVE     ZERO                TO   KJS-F12.
     MOVE     SPACE               TO   KJS-F13.
*
     START    HENKJSL2  KEY  IS  >=    KJS-F01 KJS-F98 KJS-F99
                                       KJS-F02 KJS-F03 KJS-F04
                                       KJS-F05 KJS-F06 KJS-F11
                                       KJS-F12 KJS-F13
            INVALID
                MOVE    "END"         TO   HENKJSL2-INV-FLG
            NOT INVALID
                MOVE    "   "         TO   HENKJSL2-INV-FLG
     END-START.
*
 HENKJSL2-START-EXIT.
     EXIT.
*
****************************************************************
*    ホスト返品実績ファイル読込
****************************************************************
 HENKJSL2-READ-SEC           SECTION.
*
     MOVE    "HENKJSL2-READ-SEC"  TO   S-NAME.
*
 HENKJSL2-READ-01.
     READ     HENKJSL2  AT  END
              MOVE     "END"      TO   HENKJSL2-INV-FLG
              GO                  TO   HENKJSL2-READ-EXIT
     END-READ.
*条件範囲内判定
*対象範囲チェック　倉庫ＣＤ
*  倉庫・取込日・時刻・返品Ｇ・取引先・店舗・返品日
     IF     ( KJS-F01 NOT =  KAN-F01 ) OR
            ( KJS-F98 NOT =  KAN-F98 ) OR
            ( KJS-F99 NOT =  KAN-F99 ) OR
            ( KJS-F02 NOT =  KAN-F02 ) OR
            ( KJS-F03 NOT =  KAN-F03 ) OR
            ( KJS-F04 NOT =  KAN-F04 ) OR
            ( KJS-F05 NOT =  KAN-F05 )
              MOVE  "BRK"    TO   HENKJSL2-INV-FLG
              GO             TO   HENKJSL2-READ-EXIT
     ELSE
              MOVE  "   "    TO   HENKJSL2-INV-FLG
     END-IF.
*
*件数カウント
     ADD      1                   TO   HENKJSL2-READ-CNT.
*
 HENKJSL2-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝ホスト返品管理ファイル読込件数
     DISPLAY "HENKANL2 READ-CNT  = " HENKANL2-READ-CNT UPON CONS.
*    DISPLAY "OUTPUT   PRINT-CNT = " PRINT-OUT-CNT UPON CONS.
*    DISPLAY "         ERR-CNT   = " HENKANL2-ERR-CNT UPON CONS.
     DISPLAY "         PAGE-CNT  = " PAGE-CNT UPON CONS.
*
     CLOSE     HENKANL2  HENKAML2 HENKJSL2
               ZSOKMS1   TOKMS2   TENMS1
*2019.01.28↓
*              MEIMS1    TANMS1   PRTF.
               MEIMS1    TANMS1   TANHENL1   PRTF.
*2019.01.28↑
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ索引
****************************************************************
 ZSOKMS1-READ-SEC           SECTION.
     MOVE "ZSOKMS1-READ-SEC"      TO   S-NAME.
*
     READ       ZSOKMS1
         INVALID       MOVE "INV" TO   ZSOKMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ索引
****************************************************************
 TENMS1-READ-SEC           SECTION.
     MOVE "TENMS1-READ-SEC"       TO   S-NAME.
*
     READ       TENMS1
         INVALID       MOVE "INV" TO   TENMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ索引
****************************************************************
 TOKMS2-READ-SEC           SECTION.
     MOVE "TOKMS2-READ-SEC"        TO   S-NAME.
*
     READ       TOKMS2
         INVALID       MOVE "INV" TO   TOKMS2-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ索引
****************************************************************
 MEIMS1-READ-SEC           SECTION.
     MOVE "MEIMS1-READ-SEC"       TO   S-NAME.
*
     READ       MEIMS1
         INVALID       MOVE "INV" TO   MEIMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*
*2019.01.28↓
****************************************************************
*    担当者変換マスタ索引
****************************************************************
 TANHENL1-READ-SEC           SECTION.
     MOVE "TANHENL1-READ-SEC"     TO   S-NAME.
*
     READ       TANHENL1
         INVALID       MOVE "INV" TO   TANHENL1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANHENL1-INV-FLG
     END-READ.
*
 TANHENL1-READ-EXIT.
     EXIT.
*2019.01.28↑
*-------------< PROGRAM END >------------------------------------*

```
