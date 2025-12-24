****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合　ＤＣＭＪＡＰＡＮ        *
*    モジュール名　　　　：　請求データ消し込み　　　　　　　　*
*    作成日／更新日　　　：　14/09/01                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細ファイルと請求明細ファイル*
*                        ：　の金額を照合し、一致したデータを　*
*                            削除する。（取＋店＋伝_　検索）  *
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
*    作成日／更新日　　　：　17/03/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　カーマセンター納品対応　　　　　　*
*                            （東日本出荷別取引先対応）　　　　*
*    作成日／更新日　　　：　17/12/22                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキセンター追加対応　　　　　　*
****************************************************************
*<履歴>*********************************************************
*　変更日　　変更者　　変更変更内容　　　　　　　　　　　　　　*
*  XXXXXXXX  ＮＮＮＮ　ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ*
*  20140901  高橋　　　新規作成　　　　　　　　　　　　　　　　*
*  20170321  高橋　　　カーマセンター納品対応　　　　　　　　　*
*  20171222  高橋　　　ダイキセンター追加対応　　　　　　　　　*
*  20180322  高橋　　　ケーヨー対応　　　　　　　　　　　　　　*
*  20220713  高橋　　　取ＣＤ＋店＋伝ＮＯ→取ＣＤ＋伝ＮＯへ変更*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8705S.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/29.
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
*----<< 支払合計ファイル >>--*
     SELECT   SIHARASF  ASSIGN         DA-01-S-SIHARASF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGK871  ASSIGN         DA-01-VI-SETGK871
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01
                                       SEI-F05
                        STATUS         SEI-ST.
*
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SIHARASF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  40  RECORDS.
     COPY     SIHARASF   OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGK871            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SSI8705S".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
 01  HTENMS-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  HTENMS-CHK-FLG     PIC  X(03)     VALUE  SPACE.
*#2017/03/21 NAV ST
 01  HTENMS-KM-CHK-FLG  PIC  X(03)     VALUE  SPACE.
*#2017/03/21 NAV ED
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02)     VALUE  SPACE.
 01  SEI-ST             PIC  X(02)     VALUE  SPACE.
 01  TEN-ST             PIC  X(02)     VALUE  SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  DEL-CNT            PIC  9(07)     VALUE  ZERO.
 01  ERR-CNT            PIC  9(07)     VALUE  ZERO.
 01  RD-CNT             PIC  9(07)     VALUE  ZERO.
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
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(11).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  FILLER         PIC  X(02).
     03  HEN-DENNO      PIC  X(09).
*----<< 支払金額 >>--*
 01  WK-KINGAKU         PIC S9(10)    VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT1      PIC 9(07).
 01  PARA-OUT-CNT2      PIC 9(07).
****************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-CNT1
                                       PARA-OUT-CNT2.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SIHARASF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SIHARASF  ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO             PROGRAM-STATUS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK871.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGK871  ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO             PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TEN-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " TENMS1    ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO             PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  NOT =  SPACE.
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SIHARASF.
     OPEN     I-O       SETGK871.
     OPEN     INPUT     TENMS1.
*照合ファイル初期ＲＥＡＤ
     PERFORM  900-SSI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE     SSI-F01        TO   SEI-F01.
*****MOVE     SSI-F02        TO   SEI-F03.
     MOVE     SSI-F04        TO   SEI-F05.
*D2018/03/22 NAV ST 下記、停止
*サンコーか判定
*#***IF       SSI-F01  =  100403 OR 100441 OR 100427
*D            MOVE   SSI-F01 TO   TEN-F52
*D            MOVE   SSI-F02 TO   TEN-F011
*D            PERFORM HTENMS-READ-SEC
*D            IF   HTENMS-CHK-FLG = "CHK"
*D                 ADD    1       TO  SEI-F01
*D                 IF RD-CNT(5:3) = "000" OR "500"
*D                    DISPLAY "SEI-F01 = " SEI-F01 UPON CONS
*D                 END-IF
*D            ELSE
*D*****************ダイキセンターの場合
*D2017/12/22 NAV ST センター＝４８５５追加
*D#2017/12/22      IF  SSI-F02 = 4851  OR 4852
*D                 IF  SSI-F02 = 4851  OR 4852 OR  4855
*D2017/12/22 NAV ED
*D                     ADD 1      TO  SEI-F01
*D                 END-IF
*D            END-IF
*D   END-IF.
*D2017/03/21 NAV ST
*カーマ資材判定
*D   IF       SSI-F01  =  13938
*D            MOVE   SSI-F01 TO   TEN-F52
*D            MOVE   SSI-F02 TO   TEN-F011
*D            PERFORM HTENMS-KM-READ-SEC
*D            IF   HTENMS-KM-CHK-FLG = "CHK"
*D                 MOVE 139381 TO SEI-F01
*D                 IF RD-CNT(5:3) = "000" OR "500"
*D                    DISPLAY "SEI-F01 = " SEI-F01 UPON CONS
*D                 END-IF
*D            END-IF
*D   END-IF.
**Dカーマ植物判定
*D   IF       SSI-F01  =  17137
*D            MOVE   SSI-F01 TO   TEN-F52
*D            MOVE   SSI-F02 TO   TEN-F011
*D            PERFORM HTENMS-KM-READ-SEC
*D            IF   HTENMS-KM-CHK-FLG = "CHK"
*D                 MOVE 171371 TO SEI-F01
*D                 IF RD-CNT(5:3) = "000" OR "500"
*D                    DISPLAY "SEI-F01 = " SEI-F01 UPON CONS
*D                 END-IF
*D            END-IF
*D   END-IF.
*#2017/03/21 NAV ED
*
     PERFORM  900-SEI-READ.
     IF       INV-FLG    =   SPACE
              IF  SSI-F05   =   SEI-F06
                  DELETE  SETGK871
                  ADD     1      TO   DEL-CNT
              ELSE
                  ADD     1      TO   ERR-CNT
              END-IF
     ELSE
*             カーマ／ダイキの場合、相手伝票番号でも検索
              IF  SEI-F01  =  13938 OR 17137 OR 100403 OR 100441
              OR  100427 OR 100404 OR 100442 OR 100428 OR 139381
              OR  171371
                  MOVE     SSI-F01        TO   SEI-F01
                  MOVE     SSI-F07        TO   SEI-F05
                  PERFORM  900-SEI-READ
                  IF       INV-FLG    =   SPACE
                           IF  SSI-F05   =   SEI-F06
                               DELETE  SETGK871
                               ADD     1      TO   DEL-CNT
                           ELSE
                               ADD     1      TO   ERR-CNT
                           END-IF
                  ELSE
                           CONTINUE
                  END-IF
              END-IF
     END-IF.
*
     PERFORM  900-SSI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    SIHARASF.
     CLOSE    SETGK871.
     CLOSE    TENMS1.
*削除件数表示
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
     DISPLAY "ｴﾗｰ   ｹﾝｽｳ = " ERR-CNT UPON CONS.
*## 2008/08/28 内部統制対応
     MOVE    DEL-CNT    TO   PARA-OUT-CNT1.
     MOVE    ERR-CNT    TO   PARA-OUT-CNT2.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     SETGK871
         INVALID
              MOVE      "E"           TO   INV-FLG
         NOT INVALID
              MOVE      SPACE         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　READ                       *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SIHARASF
              AT END
              MOVE      "END"         TO   END-FLG
              GO                      TO   900-SSI-READ-EXIT
              NOT AT END
              ADD        1            TO   RD-CNT
     END-READ.
*
     IF  RD-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT  UPON CONS
     END-IF.
*
 900-SSI-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込（ダイキ用）　　　　　　　　　　　*
****************************************************************
 HTENMS-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    HTENMS-CHK-FLG.
     READ    TENMS1
             INVALID
             MOVE  "INV"   TO    HTENMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HTENMS-INV-FLG
     END-READ.
*
     IF  HTENMS-INV-FLG = "INV"
         MOVE  SPACE       TO    HTENMS-CHK-FLG
     ELSE
         IF  TEN-F73 = "E1"
*************九州　ダイキ（サンコー）
             MOVE "CHK"    TO    HTENMS-CHK-FLG
         ELSE
*************本社　ダイキ
             MOVE SPACE    TO    HTENMS-CHK-FLG
         END-IF
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
*#2017/03/21 NAV ST
****************************************************************
*　　　　　　　店舗マスタ読込（カーマ用）　　　　　　　　　　　*
****************************************************************
 HTENMS-KM-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    HTENMS-CHK-FLG.
     READ    TENMS1
             INVALID
             MOVE  "INV"   TO    HTENMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HTENMS-INV-FLG
     END-READ.
*
     IF  HTENMS-INV-FLG = "INV"
         MOVE  SPACE       TO    HTENMS-KM-CHK-FLG
     ELSE
         IF  TEN-F73 NOT = SPACE
*************カーマ（東日本）
             MOVE "CHK"    TO    HTENMS-KM-CHK-FLG
         ELSE
*************カーマ（西日本）
             MOVE SPACE    TO    HTENMS-KM-CHK-FLG
         END-IF
     END-IF.
*
 HTENMS-KM-READ-EXIT.
     EXIT.
*#2017/03/21 NAV ED
*-----------------<< PROGRAM END >>----------------------------*
