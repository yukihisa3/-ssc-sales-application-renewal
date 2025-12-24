# SJR0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0060B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領データ共通変換                *
*                            　ロイヤルHC(受領・返品）)
*    作成日　　　　　　　：　2017/08/08 INOUE                  *
*    処理概要　　　　　　：　各社毎の受領返品データを、　　　  *
*                            共通データに変換する。　　　　　　*
*                                                              *
*    更新日　　　　　　　：　    /  /                          *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0060B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/08/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領データファイル
     SELECT   ROYALJYR  ASSIGN    TO        DA-01-S-ROYALJYR
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ONL-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
*    SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       MEI-F011
*                                           MEI-F012
*                       FILE STATUS    IS   MEI-STATUS.
*受領受信状況ファイル
     SELECT   COMJYOF   ASSIGN    TO        DA-01-VI-COMJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                                            JYO-F03   JYO-F04
                        FILE STATUS    IS   JYO-STATUS.
*受領返品累積ファイル
     SELECT   COMRUIF   ASSIGN    TO        DA-01-VI-COMRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUI-F03   RUI-F01
                                            RUI-F02   RUI-F04
                                            RUI-F31
                        FILE  STATUS   IS   RUI-STATUS.
*伝票区分変換テーブル
     SELECT   DENHENF   ASSIGN    TO        DA-01-VI-DENHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HEN-F01   HEN-F02
                        FILE STATUS    IS   HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領データ　
******************************************************************
 FD  ROYALJYR           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEN-REC.
     03  DEN-01                   PIC  X(02).
     03  DEN-02                   PIC  X(254).
*    COPY     XXXXXXXX  OF    XFDLIB
*             JOINING   ONL   PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF    XFDLIB
              JOINING   TEN   PREFIX.
******************************************************************
*    得意先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF    XFDLIB
              JOINING   TOK   PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF    XFDLIB
              JOINING   TBL   PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
*FD  HMEIMS             LABEL RECORD   IS   STANDARD.
*    COPY     HMEIMS    OF    XFDLIB
*             JOINING   MEI   PREFIX.
******************************************************************
*    受領受信状況ファイル
******************************************************************
 FD  COMJYOF            LABEL RECORD   IS   STANDARD.
     COPY     COMJYOF   OF    XFDLIB
              JOINING   JYO   PREFIX.
******************************************************************
*    受領返品累積ファイル
******************************************************************
 FD  COMRUIF            LABEL RECORD   IS   STANDARD.
     COPY     COMRUIF   OF    XFDLIB
              JOINING   RUI   PREFIX.
******************************************************************
*    伝票区分変換テーブル
******************************************************************
 FD  DENHENF            LABEL RECORD   IS   STANDARD.
     COPY     DENHENF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*伝票番号ブレイク判定用
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
*エラーチェック用
 01  WK-ERR-KEIJOUBI         PIC  X(01)     VALUE  SPACE.
 01  WK-ERR-GENTANKA         PIC  X(01)     VALUE  SPACE.
*カウント
 01  WK-CNT-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-CNT-JYR              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-JYR              PIC  9(07)     VALUE  ZERO.
*
 01  STR-TIME                PIC  9(04)     VALUE  ZERO.
*
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
*01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  COMJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  COMRUIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TAISYO-FLG           PIC  X(01)     VALUE  SPACE.
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
*
*伝票ヘッダーレコード（１）
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(02).
     03  WK-DEPB02          PIC  9(05).
     03  WK-DEPB03          PIC  9(03).
     03  WK-DEPB04          PIC  X(02).
     03  WK-DEPB05          PIC  9(08).
     03  WK-DEPB06          PIC  9(09).
     03  WK-DEPB07          PIC  9(08).
     03  WK-DEPB08          PIC  9(08).
     03  WK-DEPB09          PIC  9(08).
     03  WK-DEPB10          PIC  9(08).
     03  WK-DEPB11          PIC  9(08).
     03  WK-DEPB12          PIC  X(28).
     03  WK-DEPB13          PIC  9(01).
     03  WK-DEPB14          PIC  9(12).
     03  WK-DEPB15          PIC  9(09).
     03  WK-DEPB16          PIC  X(119).
     03  WK-DEPB17          PIC  9(11).
     03  WK-DEPB18          PIC  9(01).
     03  WK-DEPB19          PIC  9(06).

*伝票ヘッダレコード（２）←当ＰＧでは非使用
 01  WK-DEPC-REC.
     03  WK-DEPC01          PIC  X(01).
     03  WK-DEPC02          PIC  X(02).
     03  WK-DEPC03          PIC  X(01).
     03  WK-DEPC04          PIC  X(30).
     03  WK-DEPC05          PIC  X(20).
     03  WK-DEPC06          PIC  X(20).
     03  WK-DEPC07          PIC  X(17).
     03  WK-DEPC08          PIC  X(17).
     03  WK-DEPC09          PIC  X(12).
     03  WK-DEPC10          PIC  X(01).
     03  WK-DEPC11          PIC  X(04).
     03  WK-DEPC12          PIC  X(03).
*伝票明細レコード
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(02).
     03  WK-DEPD02          PIC  9(08).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  9(01).
     03  WK-DEPD05          PIC  X(13).
     03  WK-DEPD06          PIC  9(04).
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(05).
     03  WK-DEPD09          PIC  9(08)V99.
     03  WK-DEPD10          PIC  9(09).
     03  WK-DEPD11          PIC  9(08)V99.
     03  WK-DEPD12          PIC  9(09).
     03  WK-DEPD13          PIC  9(05).
     03  WK-DEPD14          PIC  9(08)V99.
     03  WK-DEPD15          PIC  9(09).
     03  WK-DEPD16          PIC  X(100).
     03  WK-DEPD17          PIC  X(33).
     03  WK-DEPD18          PIC  9(11).
     03  WK-DEPD19          PIC  9(01).
     03  WK-DEPD20          PIC  9(06).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ONL-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*    03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0060B".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORI              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORI.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYALJYR.
     MOVE      "ROYALJYR"   TO   AB-FILE.
     MOVE      ONL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC5           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   HMEIMS.
*    MOVE      "MEIMS1"     TO   AB-FILE.
*    MOVE      MEI-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMJYOF.
     MOVE      "COMJYOL1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIF.
     MOVE      "COMRUIL1"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENHENF.
     MOVE      "DENHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     ROYALJYR
                        HTENMS    HSHOTBL
                        HTOKMS    DENHENF.
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMJYOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     SPACE     TO        WK-HEAD-REC.
*    INITIALIZE                   WK-HEAD-REC.
*    MOVE     SPACE     TO        WK-MEISAI-REC.
*    INITIALIZE                   WK-MEISAI-REC.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:4)     TO     STR-TIME.
*データ初期READ
 INIT-01.
     READ     ROYALJYR
              AT END    MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*PARA-IN取引先のみ対象
*    IF       ONL-F013  =     PARA-TORI
*             CONTINUE
*    ELSE
*             GO              TO    INIT-01
*    END-IF.
*
*件数カウント
*    １件目ヘッダ以外読み飛ばし
     IF    DEN-01   =  "DH"
           MOVE      SPACE         TO    WK-DEPB-REC
           INITIALIZE                    WK-DEPB-REC
           MOVE      DEN-REC       TO    WK-DEPB-REC
     ELSE
           GO                      TO    INIT-01
     END-IF.
*伝票区分変換マスタ索引
     MOVE  SPACE                   TO    WK-TAISYO-FLG.
     MOVE  SPACE                   TO    WK-DENKU.
     MOVE  PARA-TORI               TO    HEN-F01.
     MOVE  WK-DEPB04               TO    HEN-F02.
     PERFORM  DENHENF-READ-SEC.
     IF  DENHENF-INV-FLG  =  SPACE
         MOVE HEN-F04              TO    WK-DENKU
         IF   HEN-F05  =  "2"
              MOVE        "1"      TO    WK-TAISYO-FLG
         END-IF
     ELSE
         EVALUATE  WK-DEPB04
             WHEN "01" WHEN "02" WHEN "03" WHEN "04" WHEN "06"
                   MOVE  "40"  TO     RUI-F51
             WHEN "13"
                   MOVE  "41"  TO     RUI-F51
             WHEN "15"
                   MOVE  "1"   TO     WK-TAISYO-FLG
             WHEN  OTHER
                   DISPLAY NC"＃伝区変換エラー　＝　" WK-DEPB04
                           UPON CONS
                   MOVE  "1"   TO     WK-TAISYO-FLG
         END-EVALUATE
     END-IF.
*
*    対象外伝票区分
*      IF   ( WK-DEPB04 =    "15" )
*             GO       TO     INIT-01
*      END-IF.
*    返品
       IF   ( WK-DEPB04 =    "13" )
*---          レコード件数カウントアップ
*---          ADD       1            TO    WK-CNT-HEN
*             伝票枚数カウントアップ
              IF   WK-DEPB05         NOT = WK-DENNO
                   ADD  1            TO    WK-DEN-HEN
                   MOVE WK-DEPB05    TO    WK-DENNO
              END-IF
       END-IF.
*    受領
       IF   ( WK-DEPB04 NOT =    "13" ) AND
            ( WK-DEPB04 NOT =    "15" )
*---          レコード件数カウントアップ
*---          ADD       1            TO    WK-CNT-JYR
*             伝票枚数カウントアップ
              IF   WK-DEPB05         NOT = WK-DENNO
                   ADD   1           TO    WK-DEN-JYR
                   MOVE WK-DEPB05    TO    WK-DENNO
              END-IF
       END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*        　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*ヘッダ処理
*    IF    DEN-01   =  "DH"
*          MOVE      SPACE         TO    WK-DEPB-REC
*          INITIALIZE                    WK-DEPB-REC
*          MOVE      DEN-REC       TO    WK-DEPB-REC
*    END-IF.
*明細処理
     IF    DEN-01   =  "DD"
*          対象伝票区分のみ
           IF     WK-DEPB04 NOT = "15"
                  MOVE      DEN-REC       TO    WK-DEPD-REC
                  PERFORM                       EDIT-SEC
           END-IF
     END-IF.
*
*    PERFORM  EDIT-SEC.
*
 MAIN-02.
     READ     ROYALJYR
                AT END
                           MOVE      9         TO  END-FG
                           GO                  TO  MAIN-EXIT
                NOT AT END
                           ADD       1         TO  RD-CNT
     END-READ.
*伝票区分変換マスタ索引
     MOVE  SPACE                   TO    WK-TAISYO-FLG.
     MOVE  SPACE                   TO    WK-DENKU.
     MOVE  PARA-TORI               TO    HEN-F01.
     MOVE  WK-DEPB04               TO    HEN-F02.
     PERFORM  DENHENF-READ-SEC.
     IF  DENHENF-INV-FLG  =  SPACE
         MOVE HEN-F04              TO    WK-DENKU
         IF   HEN-F05  =  "2"
              MOVE        "1"      TO    WK-TAISYO-FLG
         END-IF
     ELSE
         EVALUATE  WK-DEPB04
             WHEN "01" WHEN "02" WHEN "03" WHEN "04" WHEN "06"
                   MOVE  "40"  TO     RUI-F51
             WHEN "13"
                   MOVE  "41"  TO     RUI-F51
             WHEN "15"
                   MOVE  "1"   TO     WK-TAISYO-FLG
             WHEN  OTHER
                   DISPLAY NC"＃伝区変換エラー　＝　" WK-DEPB04
                           UPON CONS
                   MOVE  "1"   TO     WK-TAISYO-FLG
         END-EVALUATE
     END-IF.
*PARA-IN取引先のみ対象
*    IF       ONL-F013  =     PARA-TORI
*             CONTINUE
*    ELSE
*             GO              TO    MAIN-02
*    END-IF.
*
*件数カウント
*   対象外レコード区分
     IF  ( DEN-01   NOT =  "DH" ) AND
         ( DEN-01   NOT =  "DD" )
           GO                      TO    MAIN-02
     END-IF.
*   ヘッダ保管
     IF    DEN-01   =  "DH"
           MOVE      SPACE         TO    WK-DEPB-REC
           INITIALIZE                    WK-DEPB-REC
           MOVE      DEN-REC       TO    WK-DEPB-REC
*          対象外伝票区分
*          IF     WK-DEPB04 =    "15"
*                 GO       TO     MAIN-02
*          END-IF
     END-IF.
*   明細保管
*    IF    DEN-01   =  "DD"
*          MOVE      DEN-REC       TO    WK-DEPD-REC
*          PERFORM                       EDIT-SEC
*    END-IF.
*
*   返品伝票カウント
     IF   ( WK-DEPB04     =    "13" ) AND
          ( DEN-01        =    "DD" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-HEN
*           伝票枚数カウントアップ
            IF   WK-DEPB05         NOT = WK-DENNO
                 ADD  1            TO    WK-DEN-HEN
                 MOVE WK-DEPB05    TO    WK-DENNO
            END-IF
     END-IF.
*   受領伝票カウント
     IF   ( WK-DEPB04 NOT =    "13" ) AND
          ( WK-DEPB04 NOT =    "15" ) AND
          ( DEN-01        =    "DD" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-JYR
*           伝票枚数カウントアップ
            IF   WK-DEPB05         NOT = WK-DENNO
                 ADD  1            TO    WK-DEN-JYR
                 MOVE WK-DEPB05    TO    WK-DENNO
            END-IF
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*          　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        RUI-REC.
     INITIALIZE                       RUI-REC.
*    計上日(IN:8桁)
     IF  WK-DEPB11   NUMERIC
         MOVE        "2"            TO    LINK-IN-KBN
         MOVE        WK-DEPB11      TO    LINK-IN-YMD8
         CALL        "SKYDTCKB"  USING    LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
         IF          LINK-OUT-RET   =     ZERO
             MOVE    WK-DEPB11      TO    RUI-F08
             MOVE    " "            TO    WK-ERR-KEIJOUBI
         ELSE
             MOVE    WK-DEPB11      TO    RUI-F08
             MOVE    "1"            TO    WK-ERR-KEIJOUBI
         END-IF
     ELSE
         MOVE        ZERO           TO    RUI-F08
         MOVE        "1"            TO    WK-ERR-KEIJOUBI
     END-IF.
*取引先マスタ索引
     MOVE     PARA-TORI      TO        TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*店舗マスタ索引
     MOVE     PARA-TORI      TO        TEN-F52.
     MOVE     WK-DEPB02      TO        TEN-F011.
     PERFORM  HTENMS-READ-SEC.
*商品変換ＴＢＬ索引
     MOVE     PARA-TORI      TO        TBL-F01.
     MOVE     WK-DEPD05      TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
*原単価
     IF     ( WK-DEPD09 NOT NUMERIC ) OR
            ( WK-DEPD09 =   ZERO    )
              MOVE   "1"     TO        WK-ERR-GENTANKA
              MOVE   ZERO    TO        RUI-F35
     ELSE
              MOVE   " "     TO        WK-ERR-GENTANKA
     END-IF.
*バッチ日付
     MOVE     PARA-JDATE     TO        RUI-F01.
*バッチ時刻
     MOVE     PARA-JTIME     TO        RUI-F02.
*バッチ取引先
     MOVE     PARA-TORI      TO        RUI-F03.
*伝票番号
     MOVE     WK-DEPB05      TO        RUI-F04.
*元伝票番号
*    F05 : INIT
*相手伝票区分
     MOVE     WK-DEPB04      TO        RUI-F06.
*店舗コード
     MOVE     WK-DEPB02      TO        RUI-F07.
*計上日
*分類コード
     MOVE     WK-DEPB03      TO        RUI-F09.
*訂正区分（Ｈ）
*    F10 : INIT
*直接納品先コード
*    F11 : INIT
*コメント
*    F12 : INIT
*行番号
     MOVE     WK-DEPD03      TO        RUI-F31.
*赤黒区分
     MOVE     ZERO           TO        RUI-F32.
*相手商品コード
     MOVE     WK-DEPD05      TO        RUI-F33.
*数量
     MOVE     WK-DEPD13      TO        RUI-F34.
*    COMPUTE  RUI-F34   =    ONL-F325  /   10.
*原単価
     IF       WK-ERR-GENTANKA  NOT =  "1"
              MOVE WK-DEPD09 TO        RUI-F35
*             COMPUTE   RUI-F35    =   ONL-F315  /  100
     END-IF.
*原価金額
     MOVE     WK-DEPD14      TO        RUI-F36.
*    COMPUTE  RUI-F36   =    ONL-F316  /   100.
*商品名カナ１
     MOVE     WK-DEPD16(1:15)  TO      RUI-F37.
*商品名カナ２
     MOVE     WK-DEPD16(16:15) TO      RUI-F38.
*返品理由
*    F39 : INIT
*訂正区分（Ｍ）
*    F40 : INIT
*売単価
     MOVE     WK-DEPD10      TO        RUI-F41.
*    COMPUTE  RUI-F41   =    ONL-F317  /   100.
*自社伝票区分
     MOVE     WK-DENKU       TO        RUI-F51.
*
*サカタ商品コード
     IF   HSHOTBL-INV-FLG    NOT =    "INV"
          MOVE    TBL-F031   TO        RUI-F52
          MOVE    TBL-F0321  TO        RUI-F53
          MOVE    TBL-F0322  TO        RUI-F54
          MOVE    TBL-F0323  TO        RUI-F55
     END-IF.
*    エラー区分１（取引先マスタ）
     IF   HTOKMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F561
     END-IF.
*    エラー区分２（店舗マスタ）
     IF   HTENMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F562
     END-IF.
*    エラー区分３（計上日）
     IF   WK-ERR-KEIJOUBI  = "1"
          MOVE  "1"          TO        RUI-F563
     END-IF.
*    エラー区分４（原単価）
     IF   WK-ERR-GENTANKA  = "1"
          MOVE  "1"          TO        RUI-F564
     END-IF.
*    エラー区分５（商品変換ＴＢＬ）
     IF   HSHOTBL-INV-FLG  = "INV"
          MOVE  "1"          TO        RUI-F565
     END-IF.
*    エラー区分６（非使用）
*    F566 : INIT
*    エラー区分７（非使用）
*    F567 : INIT
*    エラー区分８（非使用）
*    F568 : INIT
*    エラー区分９（非使用）
*    F569 : INIT
*    エラー区分10（非使用）
*    F56A : INIT
*    返品累積データ作成区分
*    F57  : INIT
*    返品累積データ作成日
*    F58  : INIT
*受領返品累積ファイル出力
     WRITE  RUI-REC.
     ADD    1              TO   WRT-CNT .
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        WRT-CNT   >   ZERO
*              受領受信状況ファイル出力
               PERFORM   COMJYOF-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   NC"うち受領＝" WK-CNT-JYR NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" WK-DEN-JYR NC"枚" UPON CONS.
     DISPLAY   NC"うち返品＝" WK-CNT-HEN NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" WK-DEN-HEN NC"枚" UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     ROYALJYR  HTENMS   HSHOTBL
               HTOKMS    COMJYOF  COMRUIF   DENHENF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　受領受信状況ファイル出力
****************************************************************
 COMJYOF-WRT-SEC         SECTION.
*
     MOVE   "COMJYOF-WRT-SEC"   TO   S-NAME.
*
     ACCEPT  WK-TIME       FROM      TIME.
*
 COMJYOF-WRT-HEN.
* 返品
     IF      WK-CNT-HEN =  ZERO
             GO            TO        COMJYOF-WRT-JYR
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    PARA-TORI     TO        JYO-F03.
     MOVE    02            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
     MOVE    PARA-TORI     TO        JYO-F03.
*データ種別
     MOVE    02            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     MOVE    WK-CNT-HEN    TO        JYO-F09.
     MOVE    WK-DEN-HEN    TO        JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
 COMJYOF-WRT-JYR.
* 受領
     IF      WK-CNT-JYR =  ZERO
             GO            TO        COMJYOF-WRT-EXIT
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    PARA-TORI     TO        JYO-F03.
     MOVE    01            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
     MOVE    PARA-TORI     TO        JYO-F03.
*データ種別
     MOVE    01            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     MOVE    WK-CNT-JYR    TO        JYO-F09.
     MOVE    WK-DEN-JYR    TO        JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
     GO      TO   COMJYOF-WRT-EXIT.
*
 COMJYOF-WRT-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC         SECTION.
*
     MOVE   "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"  TO  HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ読込
****************************************************************
 HTENMS-READ-SEC         SECTION.
*
     MOVE   "HTENMS-READ-SEC"   TO   S-NAME.
*
     READ  HTENMS
           INVALID     MOVE  "INV"  TO  HTENMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE   "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ  HSHOTBL
           INVALID     MOVE  "INV"  TO  HSHOTBL-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　受領返品累積ファイル検索
****************************************************************
*COMRUIF-READ-SEC        SECTION.
*
*    MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
*    READ  COMRUIF
*          INVALID     MOVE  "INV"  TO  COMRUIF-INV-FLG
*          NOT INVALID MOVE  SPACE  TO  COMRUIF-INV-FLG
*    END-READ.
*
*COMRUIF-READ-EXIT.
*    EXIT.
****************************************************************
*　　伝票区分変換テーブル読込
****************************************************************
 DENHENF-READ-SEC        SECTION.
*
     MOVE   "DENHENF-READ-SEC"  TO   S-NAME.
*
     READ  DENHENF
           INVALID     MOVE  "INV"  TO  DENHENF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  DENHENF-INV-FLG
     END-READ.
*
 DENHENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
