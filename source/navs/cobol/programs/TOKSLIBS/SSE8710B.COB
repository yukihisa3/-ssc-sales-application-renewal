****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　請求データ抽出作成　　　
*    作成日／更新日　　　：　2011/08/02                        *
*    作成者／更新者　　　：　ＮＡＶ武井　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより取引先ＣＤで　*
*　　　　　　　　　　　　：　抽出しＤＣＭ出荷実績ファイルを    *
*                        ：　参照してＤＣＭ請求合計ファイル    *
*                        ：　（資材）・（植物）を作成する。    *
* 2011.09.07 発注日が入っていない場合は、納品日をセットする　　*
* TAKAHASHI  様に変更する。　　　　　　　　　　　　　　　　　  *
* 2017.03.09 カーマ東日本出荷対応　取引先ＣＤを追加する。　　　*
* TAKAHASHI  （取引先ＣＤ：１３９３８１、１７１３７１）　　　  *
*<履歴>*********************************************************
*　変更日　　変更者　　変更変更内容　　　　　　　　　　　　　　*
*  XXXXXXXX  ＮＮＮＮ　ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ*
*  20110907  高橋　　　発注日が入っていない場合、納品日セット　*
*  20170309  高橋　　　カーマ東日本出荷対応（くろがねや）　　　*
*  20180222  高橋　　　ケーヨー対応　　　　　　　　　　　　　　*
*  20210406  高橋　　　仕入先統合（ＤＪ＝＞ＤＮへ変更）　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE8710B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2011/08/02.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル >>-*
     SELECT   SETGKF    ASSIGN         DA-01-VI-SETGKFA1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< ＤＣＭ出荷情報ファイル >>--*
     SELECT   DNSYUKF   ASSIGN         DA-01-VI-DNSYUKL9
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DJS-K03
                                       DJS-K05
                                       DJS-K08
                                       DJS-K06
                                       DJS-K07
                        STATUS         DJS-ST.
*----<< ＤＣＭ請求合計ファイル（資材）>>--*
     SELECT   SETGK87   ASSIGN         DA-01-VI-SETGK873
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SG7-F01   SG7-F03
                                       SG7-F14   SG7-F04
                                       SG7-F05
                        STATUS         SG7-ST.
*----<< ＤＣＭ請求合計ファイル（植物）>>--*
     SELECT   SETGK88   ASSIGN         DA-01-VI-SETGK883
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SG8-F01   SG8-F03
                                       SG8-F14   SG8-F04
                                       SG8-F05
                        STATUS         SG8-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル >>-*
 FD  SETGKF             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< ＤＣＭ出荷情報ファイル >>-*
 FD  DNSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DNSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*----<< ＤＣＭ請求合計ファイル（資材）>>-*
 FD  SETGK87            LABEL RECORD   IS   STANDARD.
     COPY     SETGK87   OF        XFDLIB
              JOINING   SG7       PREFIX.
*----<< ＤＣＭ請求合計ファイル（植物）>>-*
 FD  SETGK88            LABEL RECORD   IS   STANDARD.
     COPY     SETGK88   OF        XFDLIB
              JOINING   SG8       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG           PIC  X(03)    VALUE  SPACE.
     03  DJS-INV-FLG       PIC  X(03)    VALUE  SPACE.
     03  SG87-HIT-FLG      PIC  X(01)    VALUE  SPACE.
     03  SG88-HIT-FLG      PIC  X(01)    VALUE  SPACE.
 01  COUNTERS.
     03  RD-CNT            PIC  9(07).
     03  IN01-CNT          PIC  9(07).
     03  SG873-CNT         PIC  9(07).
     03  SG883-CNT         PIC  9(07).
     03  SG7-READ-CNT      PIC  9(07).
     03  SG8-READ-CNT      PIC  9(07).
     03  SG7-ERR           PIC  9(07).
     03  SG8-ERR           PIC  9(07).
*
 01  IX1                   PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST                PIC  X(02).
 01  DJS-ST                PIC  X(02).
 01  SG7-ST                PIC  X(02).
 01  SG8-ST                PIC  X(02).
*----<< ﾜｰｸ ｴﾘｱ >>--*
*
*#2017/03/09 NAV ST
*# ↓下記に取引先ＣＤ：１３９３８１、１７１３７１を追加
*#2018/02/22 NAV ST
*# ↓下記に取引先ＣＤ：１７３、７６０　東、西を追加
 01  TBL-AREA.
*****03  SG873-TBL-AREA.
****** 05  FIL    PIC  X(40)  VALUE
******       "0000088000000882000008830001393800100403".
****** 05  FIL    PIC  X(40)  VALUE
******       "00100441001004040010044200139381XXXXXXXX".
*****03  SG873-TBL-AREA-R  REDEFINES  SG873-TBL-AREA.
****** 05  SG873-TBL       PIC  X(08)   OCCURS  10.
*****03  SG883-TBL-AREA.
****** 05  FIL    PIC  X(40)  VALUE
******       "0000142700014272000142730001713700100427".
****** 05  FIL    PIC  X(40)  VALUE
******       "0010042800171371XXXXXXXXXXXXXXXXXXXXXXXX".
*****03  SG883-TBL-AREA-R  REDEFINES  SG883-TBL-AREA.
*******05  SG883-TBL       PIC  X(08)   OCCURS  10.
     03  SG873-TBL-AREA.
       05  FIL    PIC  X(40)  VALUE
             "0000088000000882000008830001393800100403".
       05  FIL    PIC  X(40)  VALUE
             "0010044100100404001004420013938100001731".
       05  FIL    PIC  X(40)  VALUE
             "00001732XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
     03  SG873-TBL-AREA-R  REDEFINES  SG873-TBL-AREA.
       05  SG873-TBL       PIC  X(08)   OCCURS  15.
     03  SG883-TBL-AREA.
       05  FIL    PIC  X(40)  VALUE
             "0000142700014272000142730001713700100427".
       05  FIL    PIC  X(40)  VALUE
             "00100428001713710000760100007602XXXXXXXX".
       05  FIL    PIC  X(40)  VALUE
             "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
     03  SG883-TBL-AREA-R  REDEFINES  SG883-TBL-AREA.
       05  SG883-TBL       PIC  X(08)   OCCURS  15.
*#2018/02/22 NAV ED ↑
*#2017/03/09 NAV ED ↑
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE              PIC  9(06).
 01  FILLER                REDEFINES      SYS-DATE.
     03  SYS-YY            PIC  9(02).
     03  SYS-MM            PIC  9(02).
     03  SYS-DD            PIC  9(02).
 01  SYS-TIME              PIC  9(08).
 01  FILLER                REDEFINES      SYS-TIME.
     03  SYS-HH            PIC  9(02).
     03  SYS-MN            PIC  9(02).
     03  SYS-SS            PIC  9(02).
     03  SYS-MS            PIC  9(02).
*末日取得サブルーチン
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
*
 01  LINK-AREA2.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 SETGKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8710B SETGKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＤＣＭ出荷情報ファイル >>--*
 DNSYUKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DNSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8710B DNSYUKF ERROR " DJS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＤＣＭ請求合計ファイル（資材）>>--*
 SETGK87-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK87.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8710B SETGK87 ERROR " SG7-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY "SEI-F01 " SEI-F01 UPON CONS.
     DISPLAY "SEI-F03 " SEI-F03 UPON CONS.
     DISPLAY "SEI-F14 " SEI-F14 UPON CONS.
     DISPLAY "SEI-F04 " SEI-F04 UPON CONS.
     DISPLAY "SEI-F05 " SEI-F05 UPON CONS.
     STOP     RUN.
*----<< ＤＣＭ請求合計ファイル（植物）>>--*
 SETGK88-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK88.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8710B SETGK88 ERROR " SG8-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN  UNTIL  END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8710B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SETGKF  DNSYUKF.
     OPEN     I-O       SETGK87.
     OPEN     I-O       SETGK88.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
** 請求合計ファイル読込み
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    ＤＣＭ出荷情報ファイル検索
     PERFORM  DNSYUK-READ-SEC.
*
     IF  SG87-HIT-FLG  =  "Y"
         PERFORM  SG87-WRITE-SEC
     END-IF.
     IF  SG88-HIT-FLG =  "Y"
         PERFORM  SG88-WRITE-SEC
     END-IF.
** 請求合計ファイル読込み
     PERFORM  SEI-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*    ファイルクローズ
     CLOSE    SETGKF.
     CLOSE    DNSYUKF.
     CLOSE    SETGK87.
     CLOSE    SETGK88.
*
     DISPLAY "+++ ｾｲｷｭｳｺﾞｳｹｲ INPUT =" RD-CNT  " +++" UPON CONS.
     DISPLAY "+++ ﾀｲｼｮｳﾃﾞｰﾀ　ｹﾝｽｳ  =" IN01-CNT " +++" UPON CONS.
     DISPLAY "+++ ｼｻﾞｲ87     OUT   =" SG873-CNT " +++" UPON CONS.
     DISPLAY "+++ ｼｻﾞｲ87  ﾀﾞﾌﾞﾘCNT =" SG7-ERR   " +++" UPON CONS.
     DISPLAY "+++ ｼｮｸﾌﾞﾂ88   OUT   =" SG883-CNT " +++" UPON CONS.
     DISPLAY "+++ ｼｮｸﾌﾞﾂ88ﾀﾞﾌﾞﾘCNT =" SG8-ERR   " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8710B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
*
 SEI-READ-010.
     READ     SETGKF    AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SEI-READ-EXIT
              NOT  AT  END
              ADD       1         TO   RD-CNT
     END-READ.
*
     IF  RD-CNT(4:4) = "0000" OR "5000"
         DISPLAY "READ-CNT = " RD-CNT  UPON CONS
     END-IF.
*
     MOVE     SPACE     TO  SG87-HIT-FLG  SG88-HIT-FLG.
     PERFORM  VARYING  IX1  FROM  1  BY  1
                UNTIL  IX1  >  15  OR
                SG87-HIT-FLG  =  "Y"
*    資材対象取引先
              IF  SEI-F01  =  SG873-TBL(IX1)
                  MOVE  "Y"  TO     SG87-HIT-FLG
                  ADD   1    TO     IN01-CNT
              END-IF
     END-PERFORM.
     PERFORM  VARYING  IX1  FROM  1  BY  1
                UNTIL  IX1  >  15  OR
                SG88-HIT-FLG  =  "Y"
*    植物対象取引先
              IF  SEI-F01  =  SG883-TBL(IX1)
                  MOVE  "Y"  TO     SG88-HIT-FLG
                  ADD   1    TO     IN01-CNT
              END-IF
     END-PERFORM.
*
     IF  SG87-HIT-FLG  =  SPACE  AND
         SG88-HIT-FLG  =  SPACE
         GO  TO  SEI-READ-010
     END-IF.
 SEI-READ-EXIT.
     EXIT.
****************************************************************
*　　　　ＤＣＭ出荷実績ファイル検索　　　　　　　　　　　　　　*
****************************************************************
 DNSYUK-READ-SEC               SECTION.
*
     MOVE  SEI-F01          TO    DJS-K03.
     MOVE  SEI-F03          TO    DJS-K05.
     MOVE  SEI-F14          TO    DJS-K08.
     MOVE  SEI-F05          TO    DJS-K06.
     MOVE  01              TO    DJS-K07.
*
     READ     DNSYUKF
         INVALID
              MOVE  "INV"    TO   DJS-INV-FLG
         NOT  INVALID
              MOVE  SPACE    TO   DJS-INV-FLG
     END-READ.
*
 DNSYUKA-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ファイル（資材）出力
****************************************************************
 SG87-WRITE-SEC         SECTION.
*    初期化
     MOVE     SPACE        TO     SG7-REC.
     INITIALIZE                   SG7-REC.
*
     MOVE  SEI-F01         TO     SG7-F01.
     MOVE  SEI-F03         TO     SG7-F03.
     MOVE  SEI-F14         TO     SG7-F14.
*納品日がゼロの場合
     IF    SEI-F14 = SPACE
     OR    SEI-F14 = "00000000"
*    システム日付８桁変換
           MOVE    "3"        TO        LINK-IN-KBN
           MOVE     SEI-F04   TO        LINK-IN-YMD6
           CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                        LINK-IN-YMD6
                                        LINK-IN-YMD8
                                        LINK-OUT-RET
                                        LINK-OUT-YMD8
           IF  LINK-OUT-RET   =    ZERO
               MOVE LINK-OUT-YMD8  TO   SG7-F14
           ELSE
               MOVE "20"           TO   SG7-F14(1:2)
               MOVE SEI-F04        TO   SG7-F14(3:6)
           END-IF

     END-IF.
     MOVE  SEI-F04         TO     SG7-F04.
     MOVE  SEI-F05         TO     SG7-F05.
*
     IF    DJS-INV-FLG  =  SPACE
               MOVE      DJS-F03     TO   SG7-F05
     END-IF.
*****READ  SETGK87
*        INVALID
*          ADD   1         TO     SG7-READ-CNT
*        NOT  INVALID
*          ADD   1         TO     SG7-ERR
*          GO  TO          SG87-WRITE-EXIT
*****END-READ.
*
     MOVE  SEI-REC         TO     SG7-REC.
*2011.09.07 NAV ST
     IF    SG7-F14 = SPACE
     OR    SG7-F14 = "00000000"
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      SG7-F04   TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   =    ZERO
                   MOVE      LINK-OUT-YMD8  TO   SG7-F14
              ELSE
                   MOVE      ZERO           TO   SG7-F14
              END-IF
     END-IF.
*
     IF    SG7-F13 = SPACE
     OR    SG7-F13 = "00000000"
            MOVE SG7-F14   TO     SG7-F13
     END-IF.
*2011.09.07 NAV ED
     IF    DJS-INV-FLG  =  SPACE
           IF  SEI-F01  =  13938
           OR  SEI-F01  =  17137
           OR  SEI-F01  =  100403
           OR  SEI-F01  =  100404
           OR  SEI-F01  =  100441
           OR  SEI-F01  =  100442
           OR  SEI-F01  =  100427
           OR  SEI-F01  =  100428
***********#2017/03/09 NAV ST 取引先ＣＤ追加
           OR  SEI-F01  =  139381
           OR  SEI-F01  =  171371
***********#2017/03/09 NAV ED 取引先ＣＤ追加
***********#2018/02/22 NAV ST 取引先ＣＤ追加
           OR  SEI-F01  =  1731
           OR  SEI-F01  =  1732
           OR  SEI-F01  =  7601
           OR  SEI-F01  =  7602
***********#2018/02/22 NAV ED 取引先ＣＤ追加
               MOVE      DJS-F03     TO   SG7-F05
           END-IF
     END-IF.
*存在チェック
     READ  SETGK87
         INVALID
           ADD   1         TO     SG7-READ-CNT
         NOT  INVALID
           ADD   1         TO     SG7-ERR
           GO  TO          SG87-WRITE-EXIT
     END-READ.
*
     WRITE  SG7-REC.
     ADD   1    TO   SG873-CNT.

 SG87-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ファイル（植物）出力
****************************************************************
 SG88-WRITE-SEC         SECTION.
*    初期化
     MOVE     SPACE        TO     SG8-REC.
     INITIALIZE                   SG8-REC.
*
     MOVE  SEI-F01         TO     SG8-F01.
     MOVE  SEI-F03         TO     SG8-F03.
     MOVE  SEI-F14         TO     SG8-F14.
*納品日がゼロの場合
     IF    SEI-F14 = SPACE
     OR    SEI-F14 = "00000000"
*    システム日付８桁変換
           MOVE    "3"        TO        LINK-IN-KBN
           MOVE     SEI-F04   TO        LINK-IN-YMD6
           CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                        LINK-IN-YMD6
                                        LINK-IN-YMD8
                                        LINK-OUT-RET
                                        LINK-OUT-YMD8
           IF  LINK-OUT-RET   =    ZERO
               MOVE LINK-OUT-YMD8  TO   SG8-F14
           ELSE
               MOVE "20"           TO   SG8-F14(1:2)
               MOVE SEI-F04        TO   SG8-F14(3:6)
           END-IF

     END-IF.
     MOVE  SEI-F04         TO     SG8-F04.
     MOVE  SEI-F05         TO     SG8-F05.
*
     IF    DJS-INV-FLG  =  SPACE
           MOVE      DJS-F03     TO   SG8-F05
     END-IF.
*****READ  SETGK88
*        INVALID
*          ADD   1         TO     SG8-READ-CNT
*        NOT  INVALID
*          ADD   1         TO     SG8-ERR
*          GO  TO          SG88-WRITE-EXIT
*****END-READ.
*
     MOVE  SEI-REC         TO     SG8-REC.
*2011.09.07 NAV ST
     IF    SG8-F14 = SPACE
     OR    SG8-F14 = "00000000"
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      SG8-F04   TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   =    ZERO
                   MOVE      LINK-OUT-YMD8  TO   SG8-F14
              ELSE
                   MOVE      ZERO           TO   SG8-F14
              END-IF
     END-IF.
*
     IF    SG8-F13 = SPACE
     OR    SG8-F13 = "00000000"
            MOVE SG8-F14   TO     SG8-F13
     END-IF.
*2011.09.07 NAV ED
     IF    DJS-INV-FLG  =  SPACE
           IF  SEI-F01  =  13938
           OR  SEI-F01  =  17137
           OR  SEI-F01  =  100403
           OR  SEI-F01  =  100404
           OR  SEI-F01  =  100441
           OR  SEI-F01  =  100442
           OR  SEI-F01  =  100427
           OR  SEI-F01  =  100428
***********#2017/03/09 NAV ST 取引先ＣＤ追加
           OR  SEI-F01  =  139381
           OR  SEI-F01  =  171371
***********#2017/03/09 NAV ED 取引先ＣＤ追加
***********#2018/02/22 NAV ST 取引先ＣＤ追加
           OR  SEI-F01  =  1731
           OR  SEI-F01  =  1732
           OR  SEI-F01  =  7601
           OR  SEI-F01  =  7602
***********#2018/02/22 NAV ED 取引先ＣＤ追加
               MOVE      DJS-F03     TO   SG8-F05
           END-IF
     END-IF.
*存在チェック
     READ  SETGK88
         INVALID
           ADD   1         TO     SG8-READ-CNT
         NOT  INVALID
           ADD   1         TO     SG8-ERR
           GO  TO          SG88-WRITE-EXIT
     END-READ.
*
     WRITE  SG8-REC.
     ADD   1    TO   SG883-CNT.

 SG88-WRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
