****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　島忠オンライン　　　　            *
*    モジュール名　　　　：　商品変更リスト　　　　　　　　    *
*    作成日／更新日　　　：　04/02/04                          *
*    作成者／更新者　　　：　ＮＡＶ　　　                      *
*    処理概要　　　　　　：　商品変更ＤＴより商品変更リスト発行*
*    伝票フォーマット　　：　ストックホーム                    *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY1604L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/02/04.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   YA
         YB        IS   YB
         YB-22     IS   YB22.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    商品変更データＦ
     SELECT   SIMASYOK           ASSIGN    TO   SIMASYOK
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   SYO-STATUS.
*    店舗マスタ
     SELECT   HTENMS             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   RANDOM
                                 RECORD    KEY  IS   TEN-F52
                                                     TEN-F011
                                 FILE STATUS    IS   TEN-STATUS.
*    商品変換テーブル
     SELECT   HSHOTBL            ASSIGN    TO   DA-01-VI-SHOTBL1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   RANDOM
                                 RECORD    KEY  IS   TBL-F01
                                                     TBL-F02
                                 FILE STATUS    IS   TBL-STATUS.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-STATUS.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO        LP-04.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
*****<< 島忠商品変更データ >>*****
 FD  SIMASYOK
                        BLOCK CONTAINS  6    RECORDS
                        LABEL RECORD    IS   STANDARD.
                        COPY  SIMASYOK  OF   XFDLIB
                        JOINING  SYO    AS   PREFIX.
*****<< 店舗マスタ >>*****
 FD  HTENMS             BLOCK CONTAINS   8   RECORDS
                        LABEL RECORD    IS   STANDARD.
                        COPY  HTENMS    OF   XFDLIB
                        JOINING   TEN   AS   PREFIX.
*****<< 商品変換テーブル >>*****
 FD  HSHOTBL            BLOCK     CONTAINS   12  RECORDS
                        LABEL     RECORD     IS  STANDARD.
                        COPY      HSHOTBL    OF  XFDLIB
                        JOINING   TBL        AS  PREFIX.
*
*****<< 画面　ファイル >>*****
 FD  DSPFILE            LABEL RECORD    IS   OMITTED.
*
     COPY    FSY16041   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   66   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  SYO-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  TBL-STATUS               PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-STATUS               PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  SYU-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
     03  KAI-FLG                  PIC  9(01)     VALUE   ZERO.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
     03  READ-CNT                 PIC  9(07)     VALUE   ZERO.
     03  IX                       PIC  9(02)     VALUE   ZERO.
     03  IY                       PIC  9(02)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
 01  WK-SYO-F04                   PIC  X(17)     VALUE   SPACE.
 01  WK-SYO-F03                   PIC  9(03)     VALUE   ZERO.
 01  WK-DENPYO-KEI                PIC  9(09)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WK-HIDUKE-HEN.
     03  WK-HIDUKE-YY             PIC  X(04)     VALUE   SPACE.
     03  WK-HIDUKE-KU1            PIC  X(01)     VALUE   SPACE.
     03  WK-HIDUKE-MM             PIC  X(02)     VALUE   SPACE.
     03  WK-HIDUKE-KU2            PIC  X(01)     VALUE   SPACE.
     03  WK-HIDUKE-DD             PIC  X(02)     VALUE   SPACE.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"島忠商品変更書データが存在しません。".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(14)     VALUE
         NC"印刷を続けますか？　Ｙ／Ｎ".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"島忠商品変更　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"店舗マスタ　　異常！！".
 01  FILE-ERR060                  PIC  N(11)     VALUE
         NC"商品変換ＴＢＬ異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY1604L".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  島忠商品変更データ情報  ****
 01  SYO1-REC.
     03  SYO1-F01          PIC  X(03).
     03  SYO1-F02          PIC  X(08).
     03  SYO1-F03          PIC  9(08).
     03  SYO1-F04          PIC  X(17).
     03  FILLER            PIC  X(01).
     03  SYO1-F05          PIC  N(30).
     03  SYO1-F06          PIC  N(20).
     03  FILLER            PIC  X(01).
     03  SYO1-F07          PIC  X(01).
     03  SYO1-F08          PIC  X(01).
     03  SYO1-F09          PIC  9(07)V9(02).
     03  SYO1-F10          PIC  9(07).
     03  SYO1-F11          PIC  9(05).
     03  SYO1-TBL.
         05  SYO1-F12      OCCURS   20.
             07  SYO1-F121 PIC  X(01).
             07  SYO1-F122 PIC  9(03).
             07  SYO1-F123 PIC  9(07)V9(02).
             07  SYO1-F124 PIC  9(07).
             07  SYO1-F125 PIC  9(05).
     03  FILLER            PIC  X(11).
 01  SYO2-REC.
     03  SYO2-F01          PIC  X(03).
     03  SYO2-F02          PIC  X(08).
     03  SYO2-F03          PIC  9(08).
     03  SYO2-F04          PIC  X(17).
     03  FILLER            PIC  X(01).
     03  SYO2-F05          PIC  N(30).
     03  SYO2-F06          PIC  N(20).
     03  FILLER            PIC  X(01).
     03  SYO2-F07          PIC  X(01).
     03  SYO2-TBL.
         05  SYO2-F08      OCCURS   20.
             07  SYO2-F081 PIC  X(01).
             07  SYO2-F082 PIC  9(03).
             07  SYO2-F083 PIC  9(07)V9(02).
             07  SYO2-F084 PIC  9(07).
             07  SYO2-F085 PIC  9(05).
     03  FILLER            PIC  X(33).
 01  SYO3-REC.
     03  SYO3-F01          OCCURS   20.
         05  SYO3-F011     PIC  X(01).
         05  SYO3-F012     PIC  9(03).
         05  SYO3-F013     PIC  9(07)V9(02).
         05  SYO3-F014     PIC  9(07).
         05  SYO3-F015     PIC  9(05).
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  X(08)     VALUE "SSY1604L".
     03  FILLER                   PIC  X(35)     VALUE   SPACE.
     03  FILLER                   PIC  N(15)     VALUE
         NC"＜商　品　変　更　リ　ス　ト＞"
         CHARACTER  TYPE  IS  YB22.
     03  FILLER                   PIC  X(23)     VALUE   SPACE.
     03  SYS-DT-YY                PIC  9(04).
     03  FILLER                   PIC  N(01)     VALUE NC"年"
         CHARACTER  TYPE  IS  YA.
     03  SYS-DT-MM                PIC  9(02).
     03  FILLER                   PIC  N(01)     VALUE NC"月"
         CHARACTER  TYPE  IS  YA.
     03  SYS-DT-DD                PIC  9(02).
     03  FILLER                   PIC  N(01)     VALUE NC"日"
         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  PG-CNT                   PIC  ZZ9.
     03  FILLER                   PIC  N(01)     VALUE NC"頁"
         CHARACTER  TYPE  IS  YA.
*
 01  HEAD-02                      CHARACTER  TYPE  IS  YB.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE  "<".
     03  FILLER                   PIC  N(04)     VALUE
         NC"商品情報".
     03  FILLER                   PIC  X(01)     VALUE  ">".
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"事業部".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD02-JIGYO               PIC  9(03).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"変更実施日".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD02-HENKOU              PIC  X(10).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  X(06)     VALUE  "JANCD:".
     03  HD02-JANCD               PIC  X(17).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  X(06)     VALUE  "ｻｶﾀCD:".
     03  HD02-S-SYOCD             PIC  X(08).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  HD02-S-HINT1             PIC  X(05).
     03  FILLER                   PIC  X(01)     VALUE  "-".
     03  HD02-S-HINT2             PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE  "-".
     03  HD02-S-HINT3             PIC  X(01).
*
 01  HEAD-03.
     03  FILLER                   PIC  X(14)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"商品名"  CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD03-SYONM               PIC  N(30)
         CHARACTER  TYPE  IS  YB.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"規格名"  CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD03-KIKAKU              PIC  N(20)
         CHARACTER  TYPE  IS  YB.
*
 01  HEAD-04                      CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE  "<".
     03  FILLER                   PIC  N(05)     VALUE
         NC"全店標準値".
     03  FILLER                   PIC  X(01)     VALUE  ">".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"変更区分".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD04-HENCD               PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  HD04-HENNM               PIC  N(04).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価単価".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD04-GENKA               PIC  Z,ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"売価単価".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  HD04-BAIKA               PIC  Z,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"発注".
     03  FILLER                   PIC  X(04)     VALUE  "ﾛｯﾄ:".
     03  HD04-ROOTO               PIC  ZZ,ZZ9.
*
 01  SEN01.
     03  SEN01-1                  OCCURS    136.
         05  FILLER               PIC  X(01)     VALUE "=".
*
 01  SEN02.
     03  SEN02-1                  OCCURS    136.
         05  FILLER               PIC  X(01)     VALUE "-".
*
 01  SEN03.
     03  FILLER                   PIC  X(31)     VALUE   SPACE.
     03  SEN03-1                  OCCURS    105.
         05  FILLER               PIC  X(01)     VALUE "-".
*
 01  BODY-01                      CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE  "<".
     03  FILLER                   PIC  N(05)     VALUE
         NC"店舗標準値".
     03  FILLER                   PIC  X(01)     VALUE  ">".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"店舗".
     03  FILLER                   PIC  X(03)     VALUE  "CD:".
     03  BD01-TENCD               PIC  9(03).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-TENNM               PIC  N(10).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"変更区分".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  BD01-HENCD               PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  BD01-HENNM               PIC  N(04).
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価単価".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  BD01-GENKA               PIC  Z,ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"売価単価".
     03  FILLER                   PIC  X(01)     VALUE  ":".
     03  BD01-BAIKA               PIC  Z,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE  SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"発注".
     03  FILLER                   PIC  X(04)     VALUE  "ﾛｯﾄ:".
     03  BD01-ROOTO               PIC  ZZ,ZZ9.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
 000-PRINTF-ERR         SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    PRINTF.
     MOVE     "SIMASYOK"       TO   ERR-FL-ID.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 000-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    SIMASYOK.
     MOVE     "SIMASYOK"       TO   ERR-FL-ID.
     MOVE      SYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 000-DSPFILE-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DSPFILE.
     MOVE     "DSPFFILE"       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR040   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 010-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HTENMS.
     MOVE     "HTENMS  "       TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR050   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 020-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL "       TO   ERR-FL-ID.
     MOVE      TBL-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR060   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     SIMASYOK  HTENMS  HSHOTBL.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG  SYU-FLG.
     MOVE     SPACE              TO   FSY16041.
*    システム日付取得
     ACCEPT   WK-YMD    FROM     DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   WK-DATE.
*
*    伝票枚数カウント
     PERFORM  SIMASYOK-CNT-SEC  UNTIL  SYORI-FLG =  "END".
*
*    変更商品件数＝０チェック（エラーメッセージ出力）
 INIT002.
     IF    WRK-MAI  <=  0
           MOVE     1         TO     ERR-FLG
     END-IF.
*    ファイルＯＰＥＮ／ＣＬＯＳＥ
     CLOSE          SIMASYOK.
     OPEN   INPUT   SIMASYOK.
*    終了ＦＬＧ解除
     MOVE     SPACE            TO    SYU-FLG.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    島忠商品変更データ読込み
****************************************************************
 SIMASYOK-READ-SEC      SECTION.
*
     READ     SIMASYOK  AT   END
              MOVE    "END"      TO     SYORI-FLG
              GO                 TO     SIMASYOK-READ-EXIT
              NOT  AT  END
              ADD      1         TO     READ-CNT
     END-READ.
*
     IF  SYO-F07  =  "1"
         MOVE     SYO-REC        TO     SYO1-REC
         MOVE     SYO1-TBL       TO     SYO3-REC
     ELSE
         MOVE     SYO-REC        TO     SYO2-REC
         MOVE     SYO2-TBL       TO     SYO3-REC
     END-IF.
*
 SIMASYOK-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ読込み
****************************************************************
 HTENMS-READ-SEC        SECTION.
*
     MOVE     72028              TO     TEN-F52.
     MOVE     SYO3-F012(IX)      TO     TEN-F011.
     READ     HTENMS
              INVALID     MOVE ALL NC"＊"  TO  BD01-TENNM
              NOT INVALID MOVE TEN-F03     TO  BD01-TENNM
     END-READ.
*店舗ＣＤセット
     MOVE     SYO3-F012(IX)      TO      BD01-TENCD.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換テーブル読込み
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE     72028              TO     TBL-F01.
     MOVE     SYO-F04            TO     TBL-F02.
     READ     HSHOTBL
              INVALID     MOVE ALL "*"     TO  HD02-S-SYOCD
                          MOVE ALL "*"     TO  HD02-S-HINT1
                          MOVE ALL "*"     TO  HD02-S-HINT2
                          MOVE ALL "*"     TO  HD02-S-HINT3
              NOT INVALID MOVE TBL-F031    TO  HD02-S-SYOCD
                          MOVE TBL-F0321   TO  HD02-S-HINT1
                          MOVE TBL-F0322   TO  HD02-S-HINT2
                          MOVE TBL-F0323   TO  HD02-S-HINT3
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*    島忠商品変更データ商品変更カウント
****************************************************************
 SIMASYOK-CNT-SEC       SECTION.
*****島忠商品変更データ読込み
     PERFORM  SIMASYOK-READ-SEC.
*
     IF  WK-SYO-F04  NOT =  SYO-F04
     AND SYORI-FLG       =  SPACE
         ADD    1         TO   WRK-MAI
         MOVE   SYO-F04   TO   WK-SYO-F04
     END-IF.
*
 SIMASYOK-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FSY16041.
*    画面表示
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    キー入力
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*処理対象未入力時処理
     IF   MD04  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          GO       TO   MAIN-SEC
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        全プリント
         WHEN      1
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   MOVE      ZERO      TO   READ-CNT
                   MOVE      99        TO   L-CNT
                   PERFORM   FL-READ-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
     IF       PAGE-SW   =    1
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    SIMASYOK.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO   RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     SIMASYOK.
*
     IF       ERR-FLG   =    ZERO
              MOVE     "END"      TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             エラーメッセージセット                2.1        *
****************************************************************
 ERR-MSG-SEC            SECTION.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   MD05
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   MD05
              MOVE   ZERO                  TO   ERR-FLG
     END-IF.
*
 ERR-MSG-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRITE-SEC          SECTION.
*    伝票総枚数セット
     MOVE     WRK-MAI   TO        MD01.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FSY16041" TO        DSP-FMT.
*    画面表示
     WRITE    FSY16041.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-READ-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     PERFORM  SIMASYOK-READ-SEC.
*
     IF       SYORI-FLG  =  "END"
              GO                       TO   FL-READ-EXIT
     END-IF.
*    明細行印字
     PERFORM  MEISAI-WT-SEC.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*
     IF        CNT-PAGE       >    ZERO
               MOVE      SPACE     TO   PRT-REC
               WRITE     PRT-REC   AFTER     PAGE
     END-IF.
*    日付セット
     MOVE     WK-DATE(1:4)     TO      SYS-DT-YY.
     MOVE     WK-DATE(5:2)     TO      SYS-DT-MM.
     MOVE     WK-DATE(7:2)     TO      SYS-DT-DD.
*    頁セット
     ADD      1                TO      CNT-PAGE.
     MOVE     CNT-PAGE         TO      PG-CNT.
*    ヘッダー行印字
     WRITE  PRT-REC  FROM  HEAD-01  AFTER  3.
     WRITE  PRT-REC  FROM  SEN01    AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-02  AFTER  1.
     WRITE  PRT-REC  FROM  HEAD-03  AFTER  1.
     WRITE  PRT-REC  FROM  HEAD-04  AFTER  1.
     WRITE  PRT-REC  FROM  SEN01    AFTER  1.
*    行カウンタ
     MOVE   9                  TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 MEISAI-WT-SEC          SECTION.
*明細行印字
     PERFORM VARYING IX FROM 1 BY 1 UNTIL  IX > 20
         IF  L-CNT        >  56
         OR  SYO-F04  NOT =  WK-SYO-F04
*            全店情報セット
             PERFORM ZENTEN-SET-SEC
*            ヘッダー出力
             PERFORM HEAD-WT-SEC
*            キー入替え
             MOVE    SYO1-F04      TO   WK-SYO-F04
         END-IF
*        店舗ＣＤ
         MOVE   SYO3-F012(IX)      TO   BD01-TENCD
*        店舗名
         PERFORM  HTENMS-READ-SEC
*        変更区分
         MOVE    SYO3-F011(IX)     TO   BD01-HENCD
         EVALUATE SYO1-F08
             WHEN "1" MOVE NC"新規　　" TO BD01-HENNM
             WHEN "2" MOVE NC"変更　　" TO BD01-HENNM
             WHEN "3" MOVE NC"扱い中止" TO BD01-HENNM
             WHEN "4" MOVE NC"廃番　　" TO BD01-HENNM
             WHEN OTHER MOVE  SPACE     TO BD01-HENNM
         END-EVALUATE
*        原価
         MOVE   SYO3-F013(IX)      TO   BD01-GENKA
*        売価
         MOVE   SYO3-F014(IX)      TO   BD01-BAIKA
*        発注ﾛｯﾄ
         MOVE   SYO3-F015(IX)      TO   BD01-ROOTO
*        明細行出力
         IF   SYO3-F011(IX)  NOT =  SPACE
              WRITE  PRT-REC  FROM  BODY-01  AFTER  1
              WRITE  PRT-REC  FROM  SEN02    AFTER  1
         END-IF
         ADD    2                  TO   L-CNT
     END-PERFORM.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*    全店情報セット
****************************************************************
 ZENTEN-SET-SEC         SECTION.
*
*    ヘッダー初期化
*    事業部
     MOVE    SYO1-F01      TO   HD02-JIGYO.
*    変更実施日
     MOVE    SYO1-F03(1:4) TO   WK-HIDUKE-YY.
     MOVE    "/"           TO   WK-HIDUKE-KU1.
     MOVE    SYO1-F03(5:2) TO   WK-HIDUKE-MM.
     MOVE    "/"           TO   WK-HIDUKE-KU2.
     MOVE    SYO1-F03(7:2) TO   WK-HIDUKE-DD.
     MOVE    WK-HIDUKE-HEN TO   HD02-HENKOU.
*    JANCD
     MOVE    SYO1-F04      TO   HD02-JANCD.
*    サカタＣＤ
     MOVE    SYO1-F04      TO   TBL-F02.
     PERFORM HSHOTBL-READ-SEC.
*    商品名
     MOVE    SYO1-F05      TO   HD03-SYONM.
*    規格名
     MOVE    SYO1-F06      TO   HD03-KIKAKU.
*    変更区分
     MOVE    SYO1-F08      TO   HD04-HENCD.
     EVALUATE SYO1-F08
         WHEN "1" MOVE NC"新規　　" TO HD04-HENNM
         WHEN "2" MOVE NC"変更　　" TO HD04-HENNM
         WHEN "3" MOVE NC"扱い中止" TO HD04-HENNM
         WHEN "4" MOVE NC"廃番　　" TO HD04-HENNM
         WHEN OTHER MOVE  SPACE     TO HD04-HENNM
     END-EVALUATE.
*    原価
     MOVE    SYO1-F09      TO   HD04-GENKA.
*    売価
     MOVE    SYO1-F10      TO   HD04-BAIKA.
*    発注ﾛｯﾄ
     MOVE    SYO1-F11      TO   HD04-ROOTO.
*
 ZENTEN-SET-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    SIMASYOK  DSPFILE  HTENMS  HSHOTBL.
*
 END-EXIT.
     EXIT.
