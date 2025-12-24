****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　ＤＣＭＪＡＰＡＮ　請求金額リスト　*
*    作成日／更新日　　　：　2007/05/22                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各事業所の請求金額リストを発行する*
*    作成者／更新者　　　：　2010/04/06 NAV                    *
*    処理概要　　　　　　：　事業所名を商流統合に合わせ変更　　*
*                        ：　請求金額の桁数を変更　            *
****************************************************************
**履歴**********************************************************
*    2018/03/07  高橋　　ケーヨー対応追加　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE8702L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/22.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<請求金額ファイル>>-*
     SELECT   HACKENDT  ASSIGN         DA-01-S-HACKENDT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEN-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<請求金額ファイル>>----*
 FD  HACKENDT
              BLOCK CONTAINS  40  RECORDS.
     COPY     HACKENDT OF        XFDLIB
              JOINING   KEN       PREFIX.
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  KEN-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  KEN-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SSE8702L".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
 01  WK-DT2.
     03  WK-DT2-01                PIC  9(06)  VALUE  ZERO.
     03  WK-DT2-02                PIC S9(09)  VALUE  ZERO.
     03  WK-DT2-03                PIC S9(09)  VALUE  ZERO.
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  HD1-005                 PIC  N(02)
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"※※　ＤＣＭＪＰ請求金額合計リスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD15                        CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                             NC"請求締日：".
     03  HD15-SIMEBI             PIC  9(08)  VALUE  ZERO.
*
 01  HD2.
     03  FILLER                  PIC  X(39)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"事業部名"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"伝票枚数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"請求金額"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"消費税額"
                                 CHARACTER   TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(39)  VALUE  SPACE.
     03  DT1-01                  PIC  N(06).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-02                  PIC  ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-03                  PIC  ---,---,---,--9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  ---,---,---,--9.
*
 01  DT2.
     03  FILLER                  PIC  X(57)  VALUE  SPACE.
     03  DT2-01                  PIC  ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-02                  PIC  ---,---,---,--9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-03                  PIC  ---,---,---,--9.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HACKENDTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACKENDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8702L HACKENDT ERROR " KEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE8702L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN  UNTIL  END-FLG = "END".
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
     DISPLAY  "*** SSE8702L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HACKENDT.
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*
     PERFORM       HACKENDT-RD-SEC.
*ヘッダ行印字
     IF   END-FLG  NOT =  "END"
          PERFORM       HEAD-WT-SEC
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    SPACE      TO    PRT-REC.
*
     EVALUATE  KEN-F02
*2010/04/06 出力名を修正しました。 NAV ST
*********WHEN  883  WHEN  14273
*        MOVE NC"ＨＣ　北海道"  TO  DT1-01
*        WHEN  882  WHEN  14272
*        MOVE NC"ＨＣ　東北　"  TO  DT1-01
*        WHEN  880  WHEN  1427
*        MOVE NC"ＨＣ　関東　"  TO  DT1-01
*        WHEN  13938 WHEN 17137
*        MOVE NC"カーマ　　　"  TO  DT1-01
*        WHEN  100403
*********MOVE NC"ダイキ　　　"  TO  DT1-01
*2010/04/06 出力名を修正しました。 NAV ED
         WHEN 880     WHEN  882     WHEN  883
         MOVE NC"ＤＣＭ資材　"  TO  DT1-01
*#2018/03/07 NAV ST
*********WHEN 13938
         WHEN 13938  WHEN  139381  WHEN 1731 WHEN 1732
*#2018/03/07 NAV ED
         MOVE NC"ＤＣＭ資材　"  TO  DT1-01
         WHEN 100403  WHEN  100441  WHEN  100404  WHEN  100442
         MOVE NC"ＤＣＭ資材　"  TO  DT1-01
         WHEN 1427    WHEN  14272   WHEN  14273
         MOVE NC"ＤＣＭ植物　"  TO  DT1-01
*#2018/03/07 NAV ST
*********WHEN 17137
         WHEN 17137  WHEN  171371  WHEN 7601 WHEN  7602
*#2018/03/07 NAV ED
         MOVE NC"ＤＣＭ植物　"  TO  DT1-01
         WHEN 100427  WHEN  100428
         MOVE NC"ＤＣＭ植物　"  TO  DT1-01
     END-EVALUATE.
*
     MOVE    KEN-F03     TO   DT1-02.
     ADD     KEN-F03     TO   WK-DT2-01.
*
     MOVE    KEN-F04     TO   DT1-03.
     ADD     KEN-F04     TO   WK-DT2-02.
*
     MOVE    KEN-F05     TO   DT1-04.
     ADD     KEN-F05     TO   WK-DT2-03.
*
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*
     PERFORM       HACKENDT-RD-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     IF   KEN-CNT  >  ZERO
          MOVE    WK-DT2-01   TO   DT2-01
          MOVE    WK-DT2-02   TO   DT2-02
          MOVE    WK-DT2-03   TO   DT2-03
          WRITE   PRT-REC   FROM   DT2    AFTER  1
     END-IF.
*
     CLOSE    HACKENDT PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8702L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*     事業所毎請求合計ファイル読込み
*--------------------------------------------------------------*
 HACKENDT-RD-SEC             SECTION.
     READ   HACKENDT   AT  END
            MOVE  "END"  TO  END-FLG
            NOT   AT  END
            ADD    1     TO  KEN-CNT
     END-READ.
 HACKENDT-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ_
     MOVE     1                   TO        HD1-04.
***  種別名
*****EVALUATE PARA-KBN
*********WHEN "1"  MOVE  NC"資材" TO        HD1-005
*********WHEN "2"  MOVE  NC"植物" TO        HD1-005
*****END-EVALUATE.
***  請求締日セット
     MOVE     KEN-F01             TO        HD15-SIMEBI.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD15      AFTER  2.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
