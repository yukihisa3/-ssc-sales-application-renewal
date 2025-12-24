# SJR0071B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0071B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領返品データ共通変換            *
*                            　流通ＢＭＳ（受領）              *
*                            ※受領データより取込※            *
*    作成日　　　　　　　：　2017/08/15 INOUE                  *
*    処理概要　　　　　　：　各社毎の受領データを、　　　　　  *
*                            共通データに変換する。　　　　　　*
*                                                              *
*    更新日　　　　　　　：　2018/03/26 INOUE                  *
*    更新内容　　　　　　：　セキチュー追加　　　　　　　　　　*
*    更新日　　　　　　　：　2018/09/10 INOUE                  *
*    更新内容　　　　　　：　山新追加　　　　　　　　　　      *
*    更新日　　　　　　　：　2019/04/15 INOUE                  *
*    更新内容　　　　　　：　ジュンテンドー追加　　　　　      *
*    更新日　　　　　　　：　2023/01/12 TAKAHASHI              *
*    更新内容　　　　　　：　ムサシ項目転送変更（バグ）　      *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0071B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/08/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品データファイル
     SELECT   BMSJYRL1  ASSIGN    TO        DA-01-VI-BMSJYRL1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ONL-STATUS
                        RECORD    KEY       ONL-F011
                                            ONL-F012
                                            ONL-F013
                        ORGANIZATION   IS   INDEXED.
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
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
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
*    返品データ
******************************************************************
 FD  BMSJYRL1           LABEL RECORD   IS   STANDARD.
     COPY     BMSJYRF   OF    XFDLIB
              JOINING   ONL   PREFIX.
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
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF    XFDLIB
              JOINING   MEI   PREFIX.
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
 01  WK-DENNO                PIC  X(10)     VALUE  ZERO.
*取引先ブレイク判定用
 01  WK-TORI                 PIC  9(08)     VALUE  ZERO.
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
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  COMJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  COMRUIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*ヘッダレコード退避ワーク　←流用元のまま　当ＰＧでは非使用
 01  WK-HEAD-REC.
     03  HD-F01             PIC  X(01).
     03  HD-F02             PIC  9(03).
     03  HD-F03             PIC  9(04).
     03  HD-F04             PIC  9(06).
     03  HD-F05             PIC  9(06).
     03  HD-FIL             PIC  X(108).
*明細レコード退避ワーク　←流用元のまま　当ＰＧでは非使用
 01  WK-MEISAI-REC.
     03  MS-F01             PIC  X(01).
     03  MS-F02             PIC  9(02).
     03  MS-F03             PIC  X(13).
     03  MS-F04             PIC  9(06).
     03  MS-F05             PIC  X(01).
     03  MS-F06             PIC  9(06).
     03  MS-F07             PIC  9(09).
     03  MS-F08             PIC  9(09).
     03  MS-F09             PIC  9(09).
     03  MS-F10             PIC  9(02).
     03  MS-F11             PIC  X(02).
     03  MS-FIL             PIC  X(68).
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
     03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0071B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0071B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0071B".
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
*01  PARA-TORI              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME.
*                                      PARA-TORI.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYRL1.
     MOVE      "BMSJYRL1"   TO   AB-FILE.
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
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
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
     OPEN     INPUT     BMSJYRL1  HMEIMS
                        HTENMS    HSHOTBL
                        HTOKMS    DENHENF.
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMJYOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-HEAD-REC.
     INITIALIZE                   WK-HEAD-REC.
     MOVE     SPACE     TO        WK-MEISAI-REC.
     INITIALIZE                   WK-MEISAI-REC.
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
 INIT-00.
     MOVE     PARA-JDATE     TO    ONL-F011.
     MOVE     PARA-JTIME     TO    ONL-F012.
     MOVE     ZERO           TO    ONL-F013.
     START    BMSJYRL1  KEY IS >= ONL-F011 ONL-F012 ONL-F013
              INVALID   MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
     END-START.
 INIT-01.
     READ     BMSJYRL1
              AT END    MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*PARA-IN日付・時刻のみ対象
     IF    (  ONL-F011  =     PARA-JDATE ) AND
           (  ONL-F012  =     PARA-JTIME )
              CONTINUE
     ELSE
              MOVE   9        TO    END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
     END-IF.
*
*PARA-IN取引先のみ対象
*    IF       ONL-F013  =     PARA-TORI
*             CONTINUE
*    ELSE
*             GO              TO    INIT-01
*    END-IF.
*
*件数カウント
*    対象外伝票区分
*****  IF     ONL-F334      =     "60"
*****         GO            TO    INIT-01
*****  END-IF.
*　  伝票区分変換マスタチェック
*    DISPLAY "ONL-F334 = " ONL-F334 UPON CONS.
*　  島忠はスキップ
     IF       ONL-F013  =  00072028
              GO       TO    INIT-02
     END-IF.
     MOVE     ONL-F013       TO        HEN-F01.
     MOVE     ONL-F362       TO        HEN-F02.
     PERFORM  DENHENF-READ-SEC.
     IF       DENHENF-INV-FLG  NOT =  "INV"
         IF     HEN-F05  =   "2"
                GO       TO     INIT-01
         END-IF
     ELSE
                GO       TO     INIT-01
     END-IF.
*
 INIT-02.
*    受領
*    レコード件数カウントアップ
     ADD    1     TO            WK-CNT-JYR.
*    伝票枚数カウントアップ
     IF     ONL-F302      NOT = WK-DENNO
            ADD   1       TO    WK-DEN-JYR
            MOVE ONL-F302 TO    WK-DENNO
     END-IF.
*    取引先コード保管
     MOVE   ONL-F013         TO    WK-TORI.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*
     PERFORM  EDIT-SEC.
*
 MAIN-02.
     READ     BMSJYRL1
                AT END
                           MOVE      9         TO  END-FG
                           GO                  TO  MAIN-EXIT
                NOT AT END
                           ADD       1         TO  RD-CNT
     END-READ.
*PARA-IN日付・時刻のみ対象
     IF    (  ONL-F011  =     PARA-JDATE ) AND
           (  ONL-F012  =     PARA-JTIME )
              CONTINUE
     ELSE
              MOVE   9        TO    END-FG
              GO              TO    MAIN-EXIT
     END-IF.
*件数カウント
*   対象外伝票区分
*****  IF     ONL-F334      =     "60"
*****         GO            TO    MAIN-02
*****  END-IF.
*　  伝票区分変換マスタチェック
*　  島忠はスキップ
     IF       ONL-F013  =  00072028
              GO       TO    MAIN-03
     END-IF.
     MOVE     ONL-F013       TO        HEN-F01.
     MOVE     ONL-F362       TO        HEN-F02.
     PERFORM  DENHENF-READ-SEC.
     IF       DENHENF-INV-FLG  NOT =  "INV"
         IF     HEN-F05  =   "2"
                GO       TO     MAIN-02
         END-IF
     ELSE
                GO       TO     MAIN-02
     END-IF.
*
 MAIN-03.
*   受領
*    取引先ブレイクで状況ファイル出力
     IF       ONL-F013     NOT =  WK-TORI
              MOVE      RD-CNT    TO      IN-CNT
              MOVE      WRT-CNT   TO      OUT-CNT
              DISPLAY   NC"取引先Ｃ＝" WK-TORI UPON CONS
              DISPLAY   MSG-IN    UPON CONS
              DISPLAY   MSG-OUT   UPON CONS
              DISPLAY   NC"うち受領＝" WK-CNT-JYR
                        NC"件"    UPON CONS
              DISPLAY   NC"　　　　　" WK-DEN-JYR
                        NC"枚"    UPON CONS
*----         DISPLAY   NC"うち返品＝" WK-CNT-HEN
*----                   NC"件"    UPON CONS
*----         DISPLAY   NC"　　　　　" WK-DEN-HEN
*----                   NC"枚"    UPON CONS
*            受領受信状況ファイル出力
              PERFORM             COMJYOF-WRT-SEC
*            カウントクリア
              MOVE    1           TO   RD-CNT
              MOVE    ZERO        TO   WRT-CNT
                                       WK-CNT-HEN WK-CNT-JYR
                                       WK-DEN-HEN WK-DEN-JYR
*            取引先コード保管
              MOVE   ONL-F013     TO   WK-TORI
     END-IF.
*
*      レコード件数カウントアップ
*----  ADD    1     TO            WK-CNT-HEN.
       ADD    1     TO            WK-CNT-JYR.
*      伝票枚数カウントアップ
       IF     ONL-F302      NOT = WK-DENNO
*----         ADD   1       TO    WK-DEN-HEN
              ADD   1       TO    WK-DEN-JYR
              MOVE ONL-F302 TO    WK-DENNO
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
     IF  ONL-F352    NUMERIC
         MOVE        "2"            TO    LINK-IN-KBN
         MOVE        ONL-F352       TO    LINK-IN-YMD8
         CALL        "SKYDTCKB"  USING    LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
         IF          LINK-OUT-RET   =     ZERO
             MOVE    ONL-F352       TO    RUI-F08
             MOVE    " "            TO    WK-ERR-KEIJOUBI
         ELSE
             MOVE    ONL-F352       TO    RUI-F08
             MOVE    "1"            TO    WK-ERR-KEIJOUBI
         END-IF
     ELSE
         MOVE        ZERO           TO    RUI-F08
         MOVE        "1"            TO    WK-ERR-KEIJOUBI
     END-IF.
*取引先マスタ索引
     MOVE     ONL-F013       TO        TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*店舗マスタ索引
     MOVE     ONL-F013       TO        TEN-F52.
*    MOVE     ONL-F309       TO        TEN-F011.
     EVALUATE ONL-F013
        WHEN  00008737  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00033189  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087371  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087372  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087373  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087374  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087375  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00087376  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00013020  MOVE ONL-F309(1:5)   TO   TEN-F011
        WHEN  00013052  MOVE ONL-F309(1:5)   TO   TEN-F011
        WHEN  00271934  MOVE ONL-F309(1:4)   TO   TEN-F011
        WHEN  00921084  MOVE ONL-F309(1:5)   TO   TEN-F011
*↓2018/03/26
        WHEN  00321717  MOVE ONL-F309(1:5)   TO   TEN-F011
        WHEN  00926061  MOVE ONL-F309(1:5)   TO   TEN-F011
*↑2018/03/26
*↓2018/09/10
        WHEN  00001041  MOVE ONL-F309(1:5)   TO   TEN-F011
*↑2018/09/10
*↓2019/04/15
        WHEN  00005116  MOVE ONL-F309(2:5)   TO   TEN-F011
*↑2019/04/15
*↓2020/08/06
        WHEN  00072028  MOVE ONL-F309(1:3)   TO   TEN-F011
*↑2020/08/06
*↓2023/01/12
        WHEN  00116501  MOVE ONL-F309(1:4)   TO   TEN-F011
*↑2023/01/12
        WHEN  OTHER     MOVE ONL-F309(1:5)   TO   TEN-F011
     END-EVALUATE.
     PERFORM  HTENMS-READ-SEC.
*商品変換ＴＢＬ索引
     MOVE     ONL-F013       TO        TBL-F01.
     IF       ONL-F013       =         921084
*     カインズ：商品コード（ＧＴＩＮ）
              MOVE     ONL-F413(2:13)  TO    TBL-F02
     ELSE
*     カインズ以外：商品コード（発注用）
              MOVE     ONL-F414(1:13)  TO    TBL-F02
**************#2023/01/23 NAV ST
              IF  ONL-F013  =  116501 *> ムサシはインストア
                  MOVE ONL-F414(4:7)   TO    TBL-F02
              END-IF
**************#2023/01/23 NAV ED
     END-IF.
     PERFORM  HSHOTBL-READ-SEC.
*原単価
     IF     ( ONL-F448 NOT NUMERIC ) OR
            ( ONL-F448 =   ZERO    )
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
     MOVE     ONL-F013       TO        RUI-F03.
*伝票番号
*    MOVE     ONL-F302       TO        RUI-F04.
     EVALUATE ONL-F013
        WHEN  00008737  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00033189  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087371  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087372  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087373  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087374  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087375  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00087376  MOVE ONL-F302(1:7)   TO   RUI-F04
        WHEN  00013020  MOVE ONL-F302(1:9)   TO   RUI-F04
        WHEN  00013052  MOVE ONL-F302(1:9)   TO   RUI-F04
        WHEN  00271934  MOVE ONL-F302        TO   RUI-F04
        WHEN  00921084  MOVE ONL-F302(1:9)   TO   RUI-F04
*↓2018/03/26
        WHEN  00321717  MOVE ONL-F302(1:9)   TO   RUI-F04
        WHEN  00926061  MOVE ONL-F302(1:9)   TO   RUI-F04
*↑2018/03/26
*↓2018/09/10
        WHEN  00001041  MOVE ONL-F302(1:9)   TO   RUI-F04
*↑2018/09/10
*↓2019/04/15
        WHEN  00005116  MOVE ONL-F302(1:9)   TO   RUI-F04
*↑2019/04/15
*↓2019/08/21
        WHEN  00116501  MOVE ONL-F302(1:8)   TO   RUI-F04
*↑2019/08/21
*↓2020/08/06
        WHEN  00072028  MOVE ONL-F302(1:7)   TO   RUI-F04
*↑2020/08/06
        WHEN  OTHER     MOVE ONL-F302        TO   RUI-F04
     END-EVALUATE.
*元伝票番号
*    F05 : INIT
*相手伝票区分
     MOVE     ONL-F334       TO        RUI-F06.
*#2023/01/23 NAV ST
     IF  ONL-F013  =  116501
             MOVE ONL-F362   TO        RUI-F06
     END-IF.

*店舗コード
*    MOVE     ONL-F309       TO        RUI-F07.
     EVALUATE ONL-F013
        WHEN  00008737  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00033189  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087371  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087372  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087373  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087374  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087375  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00087376  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00013020  MOVE ONL-F309(1:5)   TO   RUI-F07
        WHEN  00013052  MOVE ONL-F309(1:5)   TO   RUI-F07
        WHEN  00271934  MOVE ONL-F309(1:4)   TO   RUI-F07
        WHEN  00921084  MOVE ONL-F309(1:5)   TO   RUI-F07
*↓2018/03/26
        WHEN  00321717  MOVE ONL-F309(1:5)   TO   RUI-F07
        WHEN  00926061  MOVE ONL-F309(1:5)   TO   RUI-F07
*↑2018/03/26
*↓2018/09/10
        WHEN  00001041  MOVE ONL-F309(1:5)   TO   RUI-F07
*↑2018/09/10
*↓2019/04/15
        WHEN  00005116  MOVE ONL-F309(2:5)   TO   RUI-F07
*↑2019/04/15
*↓2019/08/21
        WHEN  00116501  MOVE ONL-F309(1:4)   TO   RUI-F07
*↑2019/08/21
*↓2020/08/06
        WHEN  00072028  MOVE ONL-F309(1:3)   TO   RUI-F07
*↑2020/08/06
        WHEN  OTHER     MOVE ONL-F309        TO   RUI-F07
     END-EVALUATE.
*計上日
*分類コード
     MOVE     ONL-F345       TO        RUI-F09.
*訂正区分（Ｈ）
*    F10 : INIT
*直接納品先コード
*    F1  : INIT
*コメント
*    F12 : INIT
*行番号
*    MOVE     ONL-F402(1:2)  TO        RUI-F31.
     EVALUATE ONL-F013
        WHEN  00008737  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00033189  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087371  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087372  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087373  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087374  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087375  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00087376  MOVE ONL-F402(3:2)   TO   RUI-F31
        WHEN  00013020  MOVE ONL-F402(1:2)   TO   RUI-F31
        WHEN  00013052  MOVE ONL-F402(1:2)   TO   RUI-F31
        WHEN  00271934  MOVE ONL-F402(1:2)   TO   RUI-F31
        WHEN  00921084  MOVE ONL-F402(1:2)   TO   RUI-F31
*↓2018/03/26
        WHEN  00321717  MOVE ONL-F402(1:2)   TO   RUI-F31
        WHEN  00926061  MOVE ONL-F402(1:2)   TO   RUI-F31
*↑2018/03/26
*↓2018/09/10
        WHEN  00001041  MOVE ONL-F402(1:2)   TO   RUI-F31
*↑2018/09/10
*↓2019/04/15
        WHEN  00005116  MOVE ONL-F402(1:2)   TO   RUI-F31
*↑2019/04/15
*↓2019/08/21
        WHEN  00116501  MOVE ONL-F402(1:2)   TO   RUI-F31
*↑2019/08/21
*↓2020/08/06
        WHEN  00072028  MOVE ONL-F402(1:1)   TO   RUI-F31
*↑2020/08/06
        WHEN  OTHER     MOVE ONL-F402        TO   RUI-F31
     END-EVALUATE.
*赤黒区分
     MOVE     "0"            TO        RUI-F32.
*
*相手商品コード
     IF       ONL-F013       =         921084
*     カインズ：商品コード（ＧＴＩＮ）
              MOVE     ONL-F413(2:13)  TO    RUI-F33
     ELSE
*     カインズ以外：商品コード（発注用）
              MOVE     ONL-F414(1:13)  TO    RUI-F33
**************#2023/01/23 NAV ST
              IF  ONL-F013  =  116501 *> ムサシはインストア
                  MOVE ONL-F414(4:7)   TO    RUI-F33
              END-IF
**************#2023/01/23 NAV ED
     END-IF.
*数量
     MOVE     ONL-F459       TO        RUI-F34.
*原単価
     IF       WK-ERR-GENTANKA  NOT =  "1"
              MOVE  ONL-F448 TO        RUI-F35
     END-IF.
*原価金額
     MOVE     ONL-F447       TO        RUI-F36.
*商品名カナ１ 商品名カナ２
     MOVE     ONL-F418       TO        RUI-F37.
     MOVE     ONL-F421       TO        RUI-F38.
*　　島忠はマスタよりセット
     IF   ONL-F013           =         00072028
          IF   HSHOTBL-INV-FLG    NOT =    "INV"
               MOVE    TBL-F031   TO        MEI-F011
               MOVE    TBL-F032   TO        MEI-F012
               PERFORM HMEIMS-READ-SEC
               IF      HMEIMS-INV-FLG  NOT = "INV"
                       MOVE    MEI-F031   TO        RUI-F37
                       MOVE    MEI-F032   TO        RUI-F38
               END-IF
          END-IF
     END-IF.
*返品理由（受領からのため、セット無し）
*****MOVE     ONL-F409       TO        RUI-F39.
*訂正区分（Ｍ）
*    F40 : INIT
*売単価
     MOVE     ONL-F450       TO        RUI-F41.
*自社伝票区分
     MOVE     HEN-F04        TO        RUI-F51.
*　　島忠は固定
     IF       ONL-F013       =         00072028
              MOVE           40        TO        RUI-F51
     END-IF.
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
*              件数表示（最終取引先）　　
               MOVE      RD-CNT    TO      IN-CNT
               MOVE      WRT-CNT   TO      OUT-CNT
               DISPLAY   NC"取引先Ｃ＝" WK-TORI UPON CONS
               DISPLAY   MSG-IN    UPON CONS
               DISPLAY   MSG-OUT   UPON CONS
               DISPLAY   NC"うち受領＝" WK-CNT-JYR
                         NC"件"    UPON CONS
               DISPLAY   NC"　　　　　" WK-DEN-JYR
                         NC"枚"    UPON CONS
*----          DISPLAY   NC"うち返品＝" WK-CNT-HEN
*----                    NC"件"    UPON CONS
*----          DISPLAY   NC"　　　　　" WK-DEN-HEN
*----                    NC"枚"    UPON CONS
     END-IF.
*
*    MOVE      RD-CNT    TO      IN-CNT.
*    MOVE      WRT-CNT   TO      OUT-CNT.
*    DISPLAY   MSG-IN    UPON CONS.
*    DISPLAY   MSG-OUT   UPON CONS.
*----DISPLAY   NC"うち受領＝" WK-CNT-JYR NC"件" UPON CONS.
*----DISPLAY   NC"　　　　　" WK-DEN-JYR NC"枚" UPON CONS.
*    DISPLAY   NC"うち返品＝" WK-CNT-HEN NC"件" UPON CONS.
*    DISPLAY   NC"　　　　　" WK-DEN-HEN NC"枚" UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     BMSJYRL1  HTENMS   HSHOTBL
               HTOKMS    COMJYOF  COMRUIF  DENHENF.
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
     MOVE    WK-TORI       TO        JYO-F03.
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
     MOVE    WK-TORI       TO        JYO-F03.
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
     MOVE    WK-TORI       TO        JYO-F03.
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
     MOVE    WK-TORI       TO        JYO-F03.
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
*　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE   "HMEIMS-READ-SEC"  TO   S-NAME.
*
     READ  HMEIMS
           INVALID     MOVE  "INV"  TO  HMEIMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
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
